# TagIt

**TagIt** is an experimental CLI tool that uses large language models
(LLMs) to generate SEO-friendly tags and descriptions for blog
posts. Currently, lots of things are hard-coded for the Tarides website, but
contributions to make it more general are welcome.

## Features

- Generate SEO-optimized tags and descriptions for blog posts.
- In-place editing option with `-i` for modifying files directly.
- Fast and simple CLI interface.

## Experimental Status

TagIt is experimental and tailored for Tarides’ content. Contributions
to generalize the tool are welcome!

## Installation

Install via OPAM:

```bash
opam pin add https://github.com/samoht/tagit.git
```

Or build from source:

```bash
git clone https://github.com/samoht/tagit.git
cd tagit
dune pkg lock
dune build
```

## Usage

### Generate Tags

```bash
tagit --tags my-blog-post.md
# Output: Cybersecurity, OCaml, Software Development

```

### Generate Descriptions

```bash
tagit --description my-blog-post.md
# Output: Learn how OCaml enhances cybersecurity and boosts software development performance.

```

### In-Place Editing

Edit the blog post file directly with generated tags and descriptions:

```bash
tagit -i --tags --description my-blog-post.md
```

## Configuring the OpenAI API

TagIt currently supports only the OpenAI API for generating tags and
descriptions. To configure the API, follow these steps:

1. **Get an OpenAI API Key** If you don't have one, sign up for an
   OpenAI account and obtain an API key.

2. **Set the API Key**
   You can set your API key as an environment variable:

   ```bash
   export OPENAI_API_KEY="your-api-key-here"
   ```

   Alternatively, you can configure it in a local configuration file. Create
   a file named `~/.config/tagit/config` in your home directory:

   ```bash
   echo 'openai_api_key = "your-api-key-here"' > ~/.config/tagit/config
   ```

3. **Verify the API Key** Ensure the API key is properly set by
   running any TagIt command. If the API key is missing or incorrect,
   TagIt will prompt an error.

Once the API key is configured, TagIt will automatically use it to
generate tags and descriptions.

## License

TagIt is open-source and licensed under the MIT License.
