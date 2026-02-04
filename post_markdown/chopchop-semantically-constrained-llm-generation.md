---
pubDate: 2026-02-03
title: Advancements in Constrained Decoding with ChopChop
tags:
    - paper
    - research
    - llm
    - ai
    - structured-output
abstract: |
    Overview and discussion of the new paper ChopChip: A Programmable Framework for Semantically Constraining the Output of Language Models.
table_of_contents: true
---

## Introduction

Recently I read [ChopChop: A Programmable Framework for Semantically Constraining the Output of Language Models](https://dl.acm.org/doi/abs/10.1145/3776708) from POPL 2026.
I found the paper to be a relatively interesting forwarding of the current ideas in structured output via JSON schema to more semantic properties such as typing.
Here's the abstract:

> Language models (LMs) can generate code but cannot guarantee its correctness—often producing outputs that violate type safety, program invariants, or other semantic properties. Constrained decoding offers a solution by restricting generation to only produce programs that satisfy user-defined properties. However, existing methods are either limited to syntactic constraints or rely on brittle, ad hoc encodings of semantic properties over token sequences rather than program structure.
> 
> We present ChopChop, the first programmable framework for constraining the output of LMs with respect to semantic properties. ChopChop introduces a principled way to construct constrained decoders based on analyzing the space of programs a prefix represents. It formulates this analysis as a realizability problem which is solved via coinduction, connecting token-level generation with structural reasoning over programs. We demonstrate ChopChop's generality by using it to enforce (1) equivalence to a reference program and (2) type safety. Across a range of models and tasks, ChopChop improves success rates while maintaining practical decoding latency.

The main interesting aspects of this work are: 
- ChopChop is *programmable*
- ChopChop generalizably connects token-level constraints to *semantic constraints*

The [.grammar](https://docs.dottxt.ai/dotgrammar/) project by [.txt](https://dottxt.ai) also seems related to this, but it is proprietary.

## Constrained Decoding

At a high level, generative LLMs work like this:

```
  input text 

--( encode )-->

  input embedding

--( extrapolate )-->

  output embedding

--( decode ) -->

  output text
```

This works surprisingly well for getting sensible natural language responses to natural language queries.
But, for many applications, a structured format for the output is required. 
For example, the output might be used as the input to an automated process which requires input of a very specific format.

Of course, structure comes in many different flavors and levels of complexity.
One fairly simple framework for constrained generation been widely adopted: [structured output](https://ai.google.dev/gemini-api/docs/structured-output) via [JSON schemas](https://json-schema.org).
This framework allows the decoding of an LLM to be constrained to fit a particular JSON Schema. For example, to represent a record for a person that has name, age, and email fields we can use a JSON schema something like this:

```json
{
    "type": "object",
    "properties": {
        "name": { "type": "string" },
        "age": { "type": "number" },
        "email": { "type": "string" }
    },
    "required": ["name", "age"]
}
```

Here, the `"name"` and `"email"` fields must be strings, the `"age"` field must be a number, and only `"name"` and `"age"` are required fields.
As per the name, JSON schemas are schemas only for JSON values.
JSON schemas are schemas for standard JSON values, but the schemas themselves have several common specification (but [Open API 3](https://spec.openapis.org/oas/3.0/schema/2024-10-18.html) is probably the most common).
There are some more advanced features such as [`oneOf`](https://json-schema.org/understanding-json-schema/reference/combining).
One cool thing about JSON schemas is there is a JSON schema that is the schema for all JSON schemas.

Essentially, JSON schemas are a format for specify a grammar over JSON-shaped data.
This grammar gives rise to a parsing algorithm for checking if a string is in the grammar.
There are [standard techniques for designing such parsing algorithms to efficiently work left-to-right](https://hackernoon.com/high-performance-text-parsing-using-finite-state-machines-fsm-6d3m33j9), so that a string can be considered as a prefix to the entire value, and can be queried for "is this string a prefix of a valid value?"

Now, to connect this all to text generation with LLMs.
Once we have this prefix-checking algorithm, we can use it to filter which tokens that an LLM can decode to.
Instead of just choosing from among the most highly weighted tokens as the token to generate next in the output stream, only tokens that Maintain the output stream as a prefix of a valid output are allowed, and we choose from among the highest weight of those tokens.
This guarantees that the output stream will be valid according to the schema that determined the prefix checker.

You can also do some more fancy stuff here, like if none of the possible tokens have a very high weight, then we can try backtracking to generate a different trajectory that we might have more luck with.

This prefix checking based on first-order properties like JSON schema validation is great for many applications. I've experimented with a few myself:

- [Mtgen](https://mtgen.fly.dev) is a custom Magic the Gathering card generator, which uses structured output to structure each card's details for the sake of controlled image generation and structured querying.
- [Visual Novel Engine](https://github.com/rybla/visual-novel-engine-v2) is a platform for generating and playing interactive visual novels.  it generates and extracts a structured representation of the story and the assets involved using structured output. 
- [pinion-cyoa](https://github.com/rybla/pinion-cyoa): Pinion CYOA is a platform for generating and playing text-based choose-your-own-adventure-games. It uses structured output to maintain a structured representation of the game state and what options the player is presented with.
- [OpenAGI Branching Agent](https://github.com/rybla/openagi-branching-agent) is a project I built during [Computer Use: Hack Night at Github](https://hackersquad.io/builders/dashboard/events/cm5vmcvvf000cov0lrdb89ci8) that allows the agent to navigate a graph the corresponds to a flowchart of tasks. It decides how to navigate using structured output.
- [small-language-games](https://github.com/rybla/small-language-games/tree/main/src/app) is a collection of LLM-based experiments with a focus on structured output. For example, it includes an [LLM-powered text adventure game](https://github.com/rybla/small-language-games/tree/main/src/app/text-adventure-v3) with structured world state, an [LLM-powered UI generator](https://github.com/rybla/small-language-games/tree/main/src/app/text-to-ui) using an AST-like representation of the UI, and a [generative filesystem simulator](https://github.com/rybla/small-language-games/tree/main/src/app/generative-filesystem) where the filesystem has a structured representation that the user and the LLM both interact with via structured actions.
- [FPV Drone Hackathon Project](https://github.com/rybla/fpv_drone_hackathon_project) is a project I submitted to the the [AI+ Expo Hackathon](https://expo.scsp.ai/hackathon/), in which my team (I and a friend) won 3rd place.
- [llm-sim](https://github.com/rybla/llm-sim): LLM-sim is a simple state-action framework for using an LLM to interpret vague user instructions and structured actions, using structured output.
- [latent-state-machine](https://github.com/rybla/latent-state-machine): Latent State Machine is a demonstration of encoding the an agent's state as a state machine, where the agent navigates the state machine using structured output formatted to the transitions.

However, clearly there is a lot more to be done here because JSON schemas are extremely limited compared to the types of constraints that we would like to have on LLM outputs.

## Beyond Token-level Analysis

[ChopChop](https://dl.acm.org/doi/abs/10.1145/3776708) demonstrates a way to extend the same approach to prefix checking to more semantic properties desired of the LLM output. 

The extra piece of constrained decoding that ChopChop brings is _semantic pruning_.
All together, ChopChop has a prefix checker that calculates the space of valid ASTs that the output stream is a prefix of, and prunes that space by the semantic pruning predicate.
A token is only valid if, upon being appended to the output stream, the space of semantically valid completions is inhabited.

This framework is programmable because you can provide your own grammar and semantic pruning predicate.
The framework connects token-level constraints to AST-level constraints by prefix checking for valid ASTs, and then semantically considering those ASTs.

What's so interesting about this approach is that it is a feasible way to enforce *any* semantic predicate over the output, and ChopChop's implementation shows this can be done relatively efficiently even for complex constraints.

ChopChop demonstrates two semantic pruning examples:
- equivalent to a reference program modulo term rewriting
- well-typed

## Implementation

ChopChop uses co-induction to encode the realizability and pruning problem for a potentially infinite set of solutions.
So instead of representing the set of possible ASTs as a literal set (or list, or array), it's represented as a coinductive space.
For example, for the grammar

```hs
data Expr
    = Lit String     -- numeric literal
    | Sum Expr Expr  -- numeric sum operator
```

the coinductive space of ASTs is encoded as

```hs
data ExprSpace
    = Empty                    -- empty space
    | Union [ExprSpace]        -- union of several spaces
    | Lit Regex
    | Sum ExprSpace ExprSpace
```

This corresponds to a version space where `Union` is the union node and `Sum` is the join node.
This encoding is able to encode an infinite space of expressions with a finite representation.
It's like an extension of regular expressions to tree-structured data (in particular, ASTs defined by context free grammars).

For example, the space of all integer literal expressions is represented by `integers`:

```hs
integers = 
    let integer = Lit "[0-9]+" in
    Union [integer, Sum integers integer]
```

What's very special about these spaces is that we can write predicates over their finite representations in order to constrain the encoded infinite space.
For example, `odds` is a predicate for only the odd-valued expressions of a given space.

```hs
odds Expr       = Expr
odds (Union xs) = Union (map odds xs)
odds (Lit re)   = Lit (re `intersect` "[0-9]*[13579]")
odds (Sum l r)  = Sum (odds l) (odds r)
```

Here, `odds integers` is the space of all odd integers, finitely represented.
However, if we eagerly evaluate `odds integers`, Then the evaluation will proceed infinitely because there is a cycle in the definition of `integers`.
So, ChopChop uses a specialized solver that tracks previously expanded terms and creates an explicit cycle when a subterm is revisited, which escapes the infinite evaluation.
There is a theorem that spaces of this form that can be predicated with finite checking using this memoization specialization.


There are more details in [the ChopChop paper](https://arxiv.org/pdf/2509.00360) about the specifics of the design and implementation of the full ChopChop system.

## Results

The ChopChop team found that the semantically constrained generation to be equivalent (up to term rewriting) to a reference program dramatically improved the success rate of generating behaviorally correct programs across a few benchmarks with some open source models (DeepSeek-Coder-6.7b, CodeLlama-7B, CodeLlama-13B) in comparison to unconstrained generation or even grammar constrained generation.
See Table 1 in [the ChopChop paper](https://arxiv.org/pdf/2509.00360) for the full results.

With these improvements, there was an overhead on how many tokens need to be churned through to get successful results.
For equivalence constrained generation, the vast majority of the time, less than 20 tokens were tried per successful token. 
For well-typedness constrained generation, the vast majority of the time, less than 60 tokens were tried per successful token.
Each try was a query to the realizability solver to decide whether the token was a valid next token or not.

## Conclusions

What other kinds of predicates can be encoded as semantic pruners in ChopChop?
[The ChopChop paper](https://arxiv.org/pdf/2509.00360) mentions in the Related Work section that [abstract interpretation where there are finitely many abstract predicates can be encoded as abstract finite tree automata (AFTA)](https://arxiv.org/abs/1710.07740), which can be converted to a finitely represented coinductive predicates for use as pruners for ChopChop.
And it turns out this can be also done for infinite abstract domain via [grammar-flow analysis](https://dl.acm.org/doi/10.5555/645843.758268).
I'm looking forward to seeing that feature work done!

So if we can encode abstract interpretation in this way, then we can essentially encode any interesting semantic property about the generated output as we like.
This is so much more powerful than the initial JSON schema-based generation, because in particular now we can encode properties of the output where one part of the output relates to another part of the output in a dynamic way.
For example, if we request the LLM to generate a list of tags and then assign those tags to some items, we can require the assigned tags to actually be from the list of tags that it originally generated.
The only way to accomplish this currently is to split the generation into two parts, and dynamically use the result from the first generation to construct the schema for the second generation.

For code generation, the ability to restrict output code to only well-typed programs is an interesting prospect.
Of course, this is non-trivial work for supporting each new type system.
But the ability to implement this for any new type system means that you can give an LLM some immediate new support for a new or otherwise low resource programming language.
This is a promising development towards the perspective that LLM-assisted code generation will facilitate the development of new programming languages rather than consolidate to only a few well-known programming languages that have the most training data available.

