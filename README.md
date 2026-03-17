# 🎡 Shiny Spinner (Wheel of Fortune)

[![Shiny App](https://img.shields.io/badge/Render-ShinyApp-blue?style=flat-square&logo=r)](https://tigertang.org/wheel_of_fortune_in_shiny/) 
[![Blog Post](https://img.shields.io/badge/Read-Blog%20Post-orange?style=flat-square)](https://tigertang.org/wheel_of_fortune_in_shiny/)

A dynamic, interactive "Wheel of Fortune" application built with **R Shiny**. Originally conceived as a team-building ice-breaker, this project demonstrates how to create game-like experiences by bridging R's data visualization with web-native interactivity.

---

## ✨ Key Features & Technical "Hacks"

* **Event-Driven Plot Interaction:** Instead of a standard button, the "Spin" trigger is a centered label within the plot itself, utilizing `plot_click` coordinates to trigger reactive events.
* **State-Based Highlighting:** To overcome static plot limitations, the app uses a `ggplot2` donut chart that programmatically highlights the "winning" slice to simulate a selection.
* **JS Interop:** Leverages the `shinyjs` package to bridge R and JavaScript, triggering `confetti.js` animations and custom audio (applause) upon a successful spin.
* **Persistent Session History:** Tracks results and total spin counts across a single session using `reactiveValues`.

## 🛠️ Technical Stack

* **R Framework:** `shiny`, `shinyjs`
* **Data Vis:** `ggplot2`
* **Assets:** Custom JavaScript (`confetti.js`), CSS, and HTML5 audio tags.

## 🚀 Getting Started

To run this application locally, ensure you have R installed and run the following in your console:

```r
# Install dependencies
install.packages(c("shiny", "ggplot2", "shinyjs"))

# Run the app directly from GitHub
shiny::runGitHub("shiny_spinner", "CodingTigerTang")
```