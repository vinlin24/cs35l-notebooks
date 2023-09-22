# F22 COM SCI 35L Notes


**Course Name:** Software Construction

**Professor:** Paul R. Eggert <br>
**Section:** 1E (**TA:** Yihan Wang, **LA:** Qianli Wu)


## Course Grading


| Category              | Weight     | Notes                                            |
| --------------------- | ---------- | ------------------------------------------------ |
| Final Exam            | 30%        | Exams are open book & notes but closed computers |
| Midterm Exam          | 20%        | Exams are open book & notes but closed computers |
| Final Group Project   | 35%        | Full-stack web application with React & Node.JS  |
| Homework              | 13%        | 6 assignments                                    |
| Class Participation   | 1.5%       | Piazza, etc.                                     |
| Feedback Surveys (x2) | 0.25% (x2) | Mid-quarter and end-of-quarter LA feedback forms |


## Notebook Organization


In preparation for the final and for my own convenience later down the line, I have overhauled my notes to now be organized by topic. The original notebooks organized by the 10 weeks of lecture and discussion are still kept in the [`weeks/`](weeks/) directory, but I am no longer maintaining them. They might have some typos, errors, or are missing some information to make it altogether coherent.

Each notebook has an H1 header that describes the main topics covered in that notebook, if different from the file name. Subtopics are then organized into H2 and H3 headers. I did away with the table of contents because they were kind of ugly, redundant on GitHub and VS Code, and don't really help if they're printed (that being said, I still have hyperlinks here and there linking to other notebooks).

I tried to keep as much information as I could from my original notes, which in turn was almost a transcript of every lecture. I hope to be able to use these notebooks for my own future reference, so it's a little verbose. Think of them altogether as a textbook. I don't advise printing them out for the final due to the time crunch, but if you want to, at least they're organized with specific topics now.


## Viewing Markdown Files on VS Code


**Pro tip:** If you're viewing a source file in VS Code, you can use `Ctrl+Shift+O` to jump to a symbol in the current editor and `Ctrl+T` to jump to a symbol in the entire workspace. For Markdown, that corresponds to headers, so you can use that to preview the outline and jump around.

**Pro tip:** If you're viewing these source files on VS Code, you can use `Ctrl+Shift+V` to render the Markdown in a separate tab (or `Ctrl+K V` to open it to the side) and read that one.


## Exporting Markdown to PDF


There are [many ways to do this](https://gist.github.com/justincbagley/ec0a6334cc86e854715e459349ab1446), but I personally use the [Markdown PDF extension](https://marketplace.visualstudio.com/items?itemName=yzane.markdown-pdf) in VS Code to export my documents. After you install the extension, simply go to the `.md` file you want to export, open the command palette (Ctrl+Shift+P), and search for 'Markdown PDF: Export (pdf)'.

Documents that use LaTeX expressions have a special `<script>` element appended at the end to help this extension correctly render the math expressions, something I had trouble getting to work with the other methods.


## Contributing


If you spot any errors or would like to make improvements, feel free to open an
issue or pull request!
