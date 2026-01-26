# Contributing to labour.markets.dahsboard

🫰 Thank you for taking the time to contribute to labour.markets.dahsboard 🫰

The following is a set of guidelines for contributing to labour.markets.dahsboard, which is hosted on <https://gitlab.data.trade.gov.uk>. These are mostly guidelines and not rules. Use your best judgement and feel free to propose changes to this document in a pull request.

## Table of contents

-   [Code of Conduct](#code-of-conduct)
-   [Asking Questions](#asking-questions)
-   [Contributing](#contributing)
-   [Style Guides](#style-guides)
    -   [Commit messages](#commit-messages)
    -   [Script styleguide](#script-styleguide)
    -   [Documentation styleguide](#documentation-styleguide)
-   [Additional Notes]
    -   [Issue and PR Labels](#issue-and-pull-request-labels)

## Code of Conduct {#code-of-conduct}

This project and everyone participating in it is governed by the Department for Business and Trade [Code of Conduct](./). By participating, you are expected to uphold this code. Please report unacceptable behaviour to your line manager.

## Asking Questions {#asking-questions}

Please raise an issue using the question template or ask in the Teams 'DS Catch up' channel if it is a straight forward query. FAQs and other usage should be documented in the repository wiki pages. If this is not the case please raise an issue.

## Contributing {#contributing}

## Style Guides {#style-guides}

### Commit messages {#commit-messages}

Follow the approach outlined by [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/). The Conventional Commits specification is a lightweight convention on top of commit messages. It provides an easy set of rules for creating an explicit commit history; which makes it easier to write automated tools on top of. This convention dovetails with SemVer, by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

```         
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

The commit contains the following structural elements, to communicate intent to the consumers of your library:

-   `fix:` a commit of the type fix patches a bug in your codebase (this correlates with PATCH in Semantic Versioning).
-   `feat:` a commit of the type feat introduces a new feature to the codebase (this correlates with MINOR in Semantic Versioning).
-   `BREAKING CHANGE:` a commit that has a footer BREAKING CHANGE:, or appends a ! after the type/scope, introduces a breaking API change (correlating with MAJOR in Semantic Versioning). A BREAKING CHANGE can be part of commits of any type.
-   Types other than fix: and feat: are allowed, for example @commitlint/config-conventional (based on the Angular convention) recommends build:, chore:, ci:, docs:, style:, refactor:, perf:, test:, and others.
-   Footers other than BREAKING CHANGE: <description> may be provided and follow a convention similar to git trailer format.

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE). A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., feat(parser): add ability to parse arrays.

We perform no checks on commits to ensure that the commit messages are of this format. It is simply expected for contributors to do so.

### Script styleguide {#script-styleguide}

Follow Google's shell [style guide](https://google.github.io/styleguide/shellguide.html). Exceptions to this are allowed and it is not enforced. So long as the content remains readable these are fine. So long as the exceptions do not become the rule.

### Contributing Documentation

There are a few guidelines with documentation.

1.  Functional usage should be described on the wiki. For example, function documentation, manpages and similar should be documented here.
2.  README's should include the description of the project, how to set it up and get going and where to seek help. Detailed usage should not be included here.

Almost anything else goes so long as you can justify its inclusion.

## Additional Notes

### Issue and Pull Request Labels {#issue-and-pull-request-labels}

Please use the templates included with the repository. If you find a need for additional templates please raise an issue here or in the [cookiecuttR](https://gitlab.data.trade.gov.uk/gareth.clews/cookiecuttR) repo.
