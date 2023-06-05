#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <utility>
#include <variant>
#include <vector>

using defer = std::shared_ptr<void>;

// Global error set
enum class GlobalError : uint16_t {
  Ok = 0,
  MissingName,
  MissingLanguage,
  MissingArgument,
  DuplicateFile,
  DuplicateTag,
  DuplicateDelimiter,
  DuplicateLanguage,
  InvalidDelimiterLength,
  InvalidDelimiter,
  UnknownOption,
  OutOfMemory,
  UnexpectedToken,
  UnexpectedEof,
  DanglingEnd,
  InvalidEndDelimiter,
};

struct Location {
  uint32_t line;
  uint32_t column;
};

std::string_view delimiters = "[{<()>}]";

struct Tokenizer {
  struct Flags {
    int nl : 1;
  };

  std::string_view text;
  Flags flags{};
  uint32_t index = 0;

  struct Token {
    constexpr static const std::array<std::string_view, 8> tag_names = {
        "eof", "nl", "codeblock", "fence", "atx", "text", "begin", "end"};
    enum class Tag {
      eof,
      nl,
      codeblock,
      fence,
      atx,
      text,
      begin,
      end,
    };

    uint32_t start;
    uint32_t end;
    Tag tag;

    [[nodiscard]] auto size() const noexcept -> uint32_t {
      return this->end - this->start;
    }

    [[nodiscard]] auto slice(const std::string_view text) const noexcept
        -> std::string_view {
      return {text.begin() + this->start, this->end - this->start};
    }
  };

  Tokenizer(const std::string_view t) noexcept { text = t; }

  auto next() noexcept -> Token {
    Token token = {
        .start = this->index,
        .end = this->index,
        .tag = Token::Tag::eof,
    };

    uint32_t counter = 0;

    enum class State {
      start,
      match,
      skip,
      paste,
      maybe_paste,
    };

    State state = State::start;

    for (; this->index < this->text.size(); this->index++) {
      const auto byte = this->text[this->index];

      switch (state) {
        case State::start: {
          switch (byte) {
            case '\n': {
              token.tag = Token::Tag::nl;
              this->index += 1;
              goto tokenizer_end;
            }

            case ' ': {
              counter += 1;
              token.start += 1;
              if (this->flags.nl and counter > 3) {
                this->index += 1;
                token.tag = Token::Tag::codeblock;
                goto tokenizer_end;
              }
              break;
            }

            case '`':
              [[fallthrough]];
            case '~': {
              token.tag = Token::Tag::fence;
              state = State::match;
              counter = byte;
              break;
            }

            case '[':
              [[fallthrough]];
            case '{':
              [[fallthrough]];
            case '<':
              [[fallthrough]];
            case '(':
              [[fallthrough]];
            case ')':
              [[fallthrough]];
            case '>':
              [[fallthrough]];
            case '}':
              [[fallthrough]];
            case ']': {
              token.tag = Token::Tag::text;
              state = State::paste;
              counter = byte;
              break;
            }

            case '#': {
              if (this->flags.nl) {
                token.tag = Token::Tag::atx;
                state = State::match;
                counter = byte;
              }
              break;
            }

            default: {
              token.tag = Token::Tag::text;
              state = State::skip;
              break;
            }
          }

          break;
        }  // end start

        case State::match: {
          if (byte != counter) {
            goto tokenizer_end;
          }

          break;
        }  // end match

        case State::skip: {
          switch (byte) {
            case '[':
              [[fallthrough]];
            case '{':
              [[fallthrough]];
            case '<':
              [[fallthrough]];
            case '(':
              [[fallthrough]];
            case ')':
              [[fallthrough]];
            case '>':
              [[fallthrough]];
            case '}':
              [[fallthrough]];
            case ']': {
              state = State::maybe_paste;
              counter = byte;
              goto tokenizer_end;
            }

            case '`':
              [[fallthrough]];
            case '~':
            case '\n': {
              goto tokenizer_end;
            }

            default: {
              break;
            }
          }

          break;
        }  // end skip

        case State::maybe_paste: {
          if (byte == counter) {
            token.tag = Token::Tag::text;
            this->index -= 1;
            goto tokenizer_end;
          } else {
            switch (byte) {
              case '[':
                [[fallthrough]];
              case '{':
                [[fallthrough]];
              case '<':
                [[fallthrough]];
              case '(':
                [[fallthrough]];
              case ')':
                [[fallthrough]];
              case '>':
                [[fallthrough]];
              case '}':
                [[fallthrough]];
              case ']': {
                counter = byte;
              }

              case '\n': {
                token.tag = Token::Tag::text;
                goto tokenizer_end;
              }

              default: {
                state = State::skip;
                break;
              }
            }
          }

          break;
        }  // end maybe_paste

        case State::paste: {
          if (byte != counter) {
            if (this->index - token.start > 1) {
              const auto offset =
                  delimiters.begin() -
                  std::ranges::find(delimiters, static_cast<uint8_t>(counter));
              if (offset < (delimiters.size() / 2)) {
                token.tag = Token::Tag::begin;
              } else {
                token.tag = Token::Tag::end;
              }
              goto tokenizer_end;
            } else {
              switch (byte) {
                case '\n': {
                  goto tokenizer_end;
                }

                default: {
                  state = State::skip;
                  break;
                }
              }
            }
          }

          break;
        }  // end paste
      }
    }
  tokenizer_end:

    token.end = this->index;

    return token;
  }
};

struct Compilation {
  using Offset = uint32_t;
  struct String {
    uint32_t offset;
  };

  struct Link {
    Offset offset;
    Offset link;
    Location location;
  };

  struct Ret {};

  struct Call {
    uint16_t indent;
    std::optional<std::string_view> command;
    Offset offset;
  };

  struct Emit {
    std::string_view text;
  };

  struct Jump {
    Offset offset;
  };

  using PatchList = std::vector<Offset>;
  using SymbolMap = std::map<std::string_view, PatchList>;
  using FileMap = std::map<std::string_view, Link>;
  using TagMap = std::map<std::string_view, Link>;

  struct Namespace {
    SymbolMap symbols = {};
    TagMap tags = {};
  };

  using Instruction = std::variant<Call, Emit, Jump, Ret>;
  using NameMap = std::map<std::string_view, Namespace>;

  NameMap namespaces;
  FileMap files = {};
  FileMap local = {};
  std::vector<Instruction> program = {};
};

struct Parser {
  Tokenizer it;
  Compilation& compilation;

  enum class HeaderError {
    Ok = static_cast<uint16_t>(GlobalError::Ok),
    MissingName = static_cast<uint16_t>(GlobalError::MissingName),
    MissingLanguage = static_cast<uint16_t>(GlobalError::MissingLanguage),
    MissingArgument = static_cast<uint16_t>(GlobalError::MissingArgument),
    DuplicateFile = static_cast<uint16_t>(GlobalError::DuplicateFile),
    DuplicateTag = static_cast<uint16_t>(GlobalError::DuplicateTag),
    DuplicateDelimiter = static_cast<uint16_t>(GlobalError::DuplicateDelimiter),
    DuplicateLanguage = static_cast<uint16_t>(GlobalError::DuplicateLanguage),
    InvalidDelimiterLength =
        static_cast<uint16_t>(GlobalError::InvalidDelimiterLength),
    InvalidDelimiter = static_cast<uint16_t>(GlobalError::InvalidDelimiter),
    UnknownOption = static_cast<uint16_t>(GlobalError::UnknownOption),
  };

  enum class ParseError : uint16_t {
    Ok = static_cast<uint16_t>(GlobalError::Ok),
    OutOfMemory = static_cast<uint16_t>(GlobalError::OutOfMemory),
    DanglingEnd = static_cast<uint16_t>(GlobalError::DanglingEnd),
    InvalidEndDelimiter =
        static_cast<uint16_t>(GlobalError::InvalidEndDelimiter),

  };

  /*
  static void parse(std::string_view name_space,
                    std::string_virw text,
                    Compilation& comp) {
    auto p = Parser{
        .it = Tokenizer(text),
        .compilation = comp,
    };

    auto err = p.run(name_space);
    switch (err) {}
  }
  */

  auto skip_block(const std::string_view fence) noexcept -> void {
    while (const auto then = this->next()) {
      if (then->tag == Tokenizer::Token::Tag::fence) {
        const auto current = then->slice(this->it.text);
        if (current.starts_with(fence)) {
          return;
        }
      }
    }
  }

  auto run(const std::string_view nspace) noexcept -> ParseError {
    auto last_is_nl = false;

    while (const auto fence_token = this->next()) {
      switch (fence_token->tag) {
        case Tokenizer::Token::Tag::nl: {
          last_is_nl = true;
          break;
        }

        case Tokenizer::Token::Tag::fence: {
          const auto fence = fence_token->slice(this->it.text);
          const auto token = this->next();

          if (token == std::nullopt) {
            return ParseError::Ok;
          }

          switch (token->tag) {
            case Tokenizer::Token::Tag::nl: {
              this->skip_block(fence);
              continue;
            }

            case Tokenizer::Token::Tag::text: {
              const auto header_slice = token->slice(this->it.text);
              const auto [err0, header] = parse_zangle_header(header_slice);

              switch (err0) {
                case HeaderError::Ok:
                  break;
                default:
                  return static_cast<ParseError>(static_cast<uint16_t>(err0));
              }

              auto [err1, newline] = this->get(Tokenizer::Token::Tag::nl);
              switch (err1) {
                case GetError::Ok:
                  break;
                case GetError::UnexpectedToken:
                  return ParseError::Ok;
                case GetError::UnexpectedEof:
                  return ParseError::Ok;
              }

              auto text_block_begin = newline.end;

              const Compilation::Offset procedure =
                  this->compilation.program.size();
              const auto end_delimiter =
                  delimiters[delimiters.size() - 1 - header.delimiter];

              while (true) {
                const auto then = this->it.next();
                const auto current = then.slice(this->it.text);
                const auto text_block_end = then.start;

                switch (then.tag) {
                  case Tokenizer::Token::Tag::fence: {
                    if (then.slice(this->it.text).starts_with(fence) and
                        this->peek().tag != Tokenizer::Token::Tag::text) {
                      const auto block = this->it.text.substr(
                          text_block_begin, text_block_end - 1);
                      this->emit(block);
                      goto parse_loop_end;
                    }
                    this->ret();
                    break;
                  }

                  case Tokenizer::Token::Tag::eof: {
                    const auto block =
                        this->it.text.substr(text_block_begin, text_block_end);
                    this->emit(block);
                    this->ret();
                    break;
                  }

                  case Tokenizer::Token::Tag::begin: {
                    if (current[0] == delimiters[header.delimiter]) {
                      const auto [err0, argstr] =
                          this->get(Tokenizer::Token::Tag::text);
                      auto args = parse_call(argstr.slice(this->it.text));
                      const auto indent = then.start - newline.start;
                      const auto [err1, end] =
                          this->get(Tokenizer::Token::Tag::end);

                      const auto e = end;
                      defer _(nullptr, [&text_block_begin, e](...) {
                        text_block_begin = e.end;
                      });

                      if (end.size() != current.size() or
                          this->it.text[end.start] != end_delimiter) {
                        return ParseError::InvalidEndDelimiter;
                      }

                      if (text_block_begin != text_block_end) {
                        const auto block = this->it.text.substr(
                            text_block_begin, text_block_end);
                        this->emit(block);
                      }

                      const auto ns =
                          args.nspace == std::nullopt ? nspace : *args.nspace;

                      this->call(indent, args.tag, ns, args.command);
                    }
                    break;
                  }

                  case Tokenizer::Token::Tag::end: {
                    if (this->it.text[then.start] == end_delimiter) {
                      return ParseError::DanglingEnd;
                    }
                    break;
                  }

                  case Tokenizer::Token::Tag::nl: {
                    newline = then;
                    break;
                  }

                  default:
                    break;
                }
              }

            parse_loop_end:
              const Compilation::Offset link =
                  this->compilation.program.size() - 1;

              const auto [space, _] =
                  this->compilation.namespaces.insert({nspace, {}});

              Compilation::Link* entry;
              bool found_existing = false;

              if (header.tag != std::nullopt) {
                const auto [val, ok] =
                    space->second.tags.insert({*header.tag, {}});
                entry = &val->second;
                found_existing = !ok;
              } else if (header.tag != std::nullopt) {
                const auto [val, ok] =
                    this->compilation.local.insert({*header.tag, {}});
                entry = &val->second;
                found_existing = !ok;
              } else {
                assert(false);
              }

              if (found_existing) {
                this->compilation.program[entry->link] = Compilation::Jump{
                    .offset = procedure,
                };
                entry->link = link;
              } else {
                *entry = {
                    .offset = procedure,
                    .link = link,
                };
              }

              break;
            }

            default:
              break;
          }

          break;
        }

        default: {
          break;
        }
      }
    }

    return ParseError::Ok;
  }

  enum class GetError {
    Ok = static_cast<uint16_t>(GlobalError::Ok),
    UnexpectedEof = static_cast<uint16_t>(GlobalError::UnexpectedEof),
    UnexpectedToken = static_cast<uint16_t>(GlobalError::UnexpectedToken),
  };

  struct GetResult {
    GetError err;
    Tokenizer::Token val;
  };

  [[nodiscard]] auto get(const Tokenizer::Token::Tag tag) noexcept
      -> GetResult {
    const auto token = this->next();

    if (token == std::nullopt)
      return GetResult{.err = GetError::UnexpectedEof};
    if (token->tag != tag)
      return GetResult{.err = GetError::UnexpectedToken};

    return GetResult{.err = GetError::Ok, .val = *token};
  }

  auto peek() noexcept -> Tokenizer::Token {
    const auto reset = this->it.index;
    const auto token = this->it.next();
    this->it.index = reset;

    return token;
  }

  struct Header {
    std::string_view lang;
    std::optional<std::string_view> file;
    std::optional<std::string_view> tag;
    char delimiter;
  };

  [[nodiscard]] static auto parse_zangle_header(
      const std::string_view text) noexcept -> std::pair<HeaderError, Header> {
    std::optional<std::string_view> file = std::nullopt;
    std::optional<std::string_view> tag = std::nullopt;
    // std::optional<char> delimiter = std::nullopt;

    uint32_t position = 0;
    while (position < text.size()) {
      auto from = text.find_first_not_of(" ", position);
      if (from == std::string_view::npos) {
        break;
      }

      auto to = text.find_first_of(" ", position + 1);
      if (to == std::string_view::npos) {
        to = text.size();
      }

      const auto command = text.substr(from, to);

      if (command == "file:") {
        std::cout << "file:" << std::endl;
      } else if (command == "tag:") {
        std::cout << "tag:" << std::endl;
      } else if (command == "del:") {
        std::cout << "del:" << std::endl;
      } else {
        std::cout << "hopefully an argument" << std::endl;
      }
    }

    return std::pair<HeaderError, Header>(
        HeaderError::Ok,
        {.lang = "zig", .file = file, .tag = tag, .delimiter = 0});
  }

  [[nodiscard]] auto next() noexcept -> std::optional<Tokenizer::Token> {
    const auto token = this->it.next();
    if (token.tag != Tokenizer::Token::Tag::eof) {
      return token;
    } else {
      return std::nullopt;
    }
  }

  struct Call {
    std::string_view tag;
    std::optional<std::string_view> nspace;
    std::optional<std::string_view> command;
  };

  [[nodiscard]] auto parse_call(const std::string_view text) noexcept -> Call {}

  auto ret() noexcept -> void {
    this->compilation.program.emplace_back(Compilation::Ret{});
  }

  auto emit(const std::string_view text) noexcept -> void {
    this->compilation.program.emplace_back(Compilation::Emit{
        .text = text,
    });
  }

  auto call(uint16_t indent,
            const std::string_view tag,
            const std::string_view nspace,
            const std::optional<std::string_view> command) noexcept -> void {
    const auto [ns, _] = this->compilation.namespaces.insert({nspace, {}});
    const auto [sym, __] = ns->second.symbols.insert({tag, {}});

    const auto offset = this->compilation.program.size();
    sym->second.emplace_back(offset);
    this->compilation.program.emplace_back(Compilation::Call{
        .indent = indent,
        .command = command,
        .offset = 0xffff'ffff,
    });
  }
};

struct Interface {
  Interface() = default;

  enum class Error {
    Ok = static_cast<uint16_t>(GlobalError::Ok),
    OutOfMemory = static_cast<uint16_t>(GlobalError::OutOfMemory),
  };

  virtual auto indent(uint16_t) noexcept -> Error { return Error::Ok; }

  virtual auto write(const std::string_view) noexcept -> Error {
    return Error::Ok;
  }

  virtual auto call(Compilation::Offset, uint16_t) noexcept -> Error {
    return Error::Ok;
  }

  virtual auto jump(Compilation::Offset) noexcept -> Error { return Error::Ok; }

  virtual auto term() noexcept -> Error { return Error::Ok; }

  virtual auto ret() noexcept -> Error { return Error::Ok; }
};

struct Interpreter {
  struct Frame {
    Compilation::Offset ip;
    uint16_t indent;
  };

  struct Flags {
    int should_indent : 1 = 0;
    int last_is_newline : 1 = 0;
  };

  enum class Status { running, done };

  std::span<Compilation::Instruction> program;
  std::vector<Frame> stack = {};
  Flags flags = {};
  uint16_t indent = 0;
  Compilation::Offset ip;

  struct RunResult {
    Interface::Error err = Interface::Error::Ok;
    Status val;
  };

  [[nodiscard]] auto step(Interface& iface) noexcept -> RunResult {
    const auto instruction = this->program[this->ip];

    // std::visit is an ugly mess, wait for a P1371R2-like match
    if (auto* v = std::get_if<Compilation::Ret>(&instruction)) {
      return this->ret(iface);
    } else if (auto* v = std::get_if<Compilation::Call>(&instruction)) {
    } else if (auto* v = std::get_if<Compilation::Emit>(&instruction)) {
    } else if (auto* v = std::get_if<Compilation::Jump>(&instruction)) {
    }

    return {.val = Status::running};
  }

  [[nodiscard]] auto ret(Interface& iface) noexcept -> RunResult {
    if (auto* frame = this->pop()) {
      this->ip = frame->ip;
      this->indent = frame->indent;

      const auto err = iface.ret();
      return {.err = err, .val = Status::running};
    } else {
      const auto err = iface.term();
      return {.err = err, .val = Status::done};
    }
  }

  [[nodiscard]] auto jump(Compilation::Jump params, Interface& iface) noexcept
      -> Interface::Error {
    const auto err0 = iface.jump(params.offset);
    if (err0 != Interface::Error::Ok)
      return err0;

    const auto err1 = iface.write("\n");
    if (err1 != Interface::Error::Ok)
      return err1;

    this->ip = params.offset;
    this->flags.last_is_newline = true;
  }

  [[nodiscard]] auto call(Compilation::Call params, Interface& iface)
      -> Interface::Error {
    {
      const auto err = this->push({
          .ip = this->ip,
          .indent = this->indent,
      });

      if (err != StackError::Ok)
        return Interface::Error::OutOfMemory;
    }

    const auto err = iface.call(params.offset, params.indent);
    switch (err) {
      case Interface::Error::Ok:
        break;
      default:
        return err;
    }

    this->indent += params.indent;
    this->ip = params.offset;

    this->flags.should_indent = false;
  }
};

// std::visit(overloaded{
//   [](Ret&) { ... },
//   [](Call&) { ... },
//   [](Emit&) { ... },
//   [](Jump&) { ... },
// }, item)

auto main(int argc, const char** argv) -> int {
  auto fd = open(argv[1], 0);
  defer _(nullptr, [fd](...) { close(fd); });

  struct stat tmp;
  fstat(fd, &tmp);

  const auto source = static_cast<char*>(
      mmap(nullptr, tmp.st_size, PROT_READ, MAP_PRIVATE, fd, 0));
  defer __(nullptr, [source, tmp](...) { munmap(source, tmp.st_size); });
  const auto text = std::string_view(source, tmp.st_size);

  Tokenizer it(text);

  std::cout << "starting loop" << std::endl;

  while (true) {
    const auto token = it.next();
    const auto n =
        static_cast<std::underlying_type<Tokenizer::Token::Tag>::type>(
            token.tag);
    std::cout << Tokenizer::Token::tag_names[n] << std::endl;
    if (token.tag == Tokenizer::Token::Tag::eof) {
      break;
    }
  }
}
