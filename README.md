# DAIMPL 2025 tabular-rdt

This is a project for the [DAIMPL 2025 course](https://www.stg.tu-darmstadt.de/teaching_stg/courses_stg/ws_2021___1/design_and_implementation_of_modern_programming_languages_2/design_and_implementation_of_modern_programming_languages_11.en.jsp).

## Motivation
Replicated data types (RDTs) such as CRDTs are becoming increasingly popular as a simplified way for programming distributed systems. RDTs are local data structures with a familiar interface (such as sets, lists, trees) that can automatically synchronize data between multiple devices in the background. For more background on CRDTs, see https://crdt.tech/.

CRDTs for simple data types like sets or lists are well understood, however, modern collaborative applications such as Notion or Google Docs also include more complicated application specific data structures such as tables/spreadsheets. In a spreadsheet, we have certain dependencies between rows and columns and a spreadsheet CRDT algorithm has to decide what happens if multiple devices edit them concurrently and potentially produce conflicts.

## Project Structure

This project is a Scala.js project and is split into two modules:

* `lib`: Provides the CRDT and the underlying data structures based on [REScala](https://www.rescala-lang.com/).
* `app`: A spreadsheet web application built with [scalajs-react](https://japgolly.github.io/scalajs-react/) to test the synchronization of the CRDT.

## Building the webapp

```
sbt "project app" fastOptJS
```
You may now open [index.html](app/index.html) in your favorite browser

## Inspiration
The implementation is inspired by the following paper, but it is not a direct implementation of it: [A CRDT for formulas in spreadsheets](https://dlnext.acm.org/doi/10.1145/3578358.3591324)
