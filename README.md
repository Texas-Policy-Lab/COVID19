
<!-- README.md is generated from README.Rmd. Please edit that file -->
Instructions
============

Load the package `library(tpltemplates); create_obj.dashboard()` or use a namespace `tpltemplates::create_obj.dashboard()` function to load the TPL dashboard template.

The template will copy `app.R` and associated files from the templates which are necessary to create the TPL dashboard. Style sheets have been included to give the dashboard a consistent look with the TPL brand.

Included documents and files:
-----------------------------

The following files will be copied into the current project:

-   .gitignore
-   config/
-   mainDashboard.yaml
-   css/
-   footer.css
-   main.css
-   mainDashboard.css
-   navBar.css
-   normalize.css
-   tabBoxes.css
-   widgets.css
-   images/
-   favicon.png
-   R/
-   app.R
-   assertions.R
-   footer.R
-   server.R
-   tab.R
-   ui.R
-   Rmd/
-   dashboard\_instructions.Rmd
-   README.Rmd
-   svg/
-   tpl\_logo\_color.svg
-   tpl\_star\_red.svg

Run the App
-----------

The `app.Rmd` will be located in the main project directory. Please use the function `shiny::runApp()` to run the application.

Edit the App
------------

### Edit Dashboard Title

Edit the `mainDashboard.yaml` configuration file. Change the parameter ui &gt; dashboardtitle &gt; title.

### Edit Dashboard Navigation

Edit the `mainDashboard.yaml` configuration file. Change the parameter ui &gt; tabs &gt; tabx &gt; menu\_title to change name of the tab on the navigation pane. Change the parameter ui &gt; tabs &gt; tabx &gt; icon to change the icon of the tab on the navigation pane. Change the parameter ui &gt; tabs &gt; tabx &gt; page\_title to change to title that appears at the top of each page.

To add more tabs to the navbar menu, add an additional tab to ui &gt; tab and follow the same format as the above tabs. To delete tabs from the navbar menu, delete tabs that are not necessary.

### Add User Interface Elements

Do not edit `app.R` or `ui.R` if you can help it. These R scripts have been set-up to work together. To edit the dashboard user interface (UI) edit the `tab.R` page. Each `ui_element.tab1()` function corresponds to the tabs in the navbar pane. Add the UI elements to appear on each tab to the ui\_elements.tabx functions.

If additional tabs are added, then uncomment or copy and paste ui\_elements.tbx to match the configuration file.

Shiny tips for success
----------------------

-   Use configuration files. Try not to hardcode any names or titles. These are always subject to change. Parameters that we expect to change frequently should be located in a configuration file which is easy to access, rather than buried in UI or server code which will take a while to find. YAML files are my preferred configuration file because you can add comments. JSON is another type of common configuration files. You will often see JSON files used with webapps because JSON files are used with JavaScript (JSON stands for JavaScript Object Notation).

-   Keep everything a modular as possible. One of the horrors of debugging an R-shiny app is trying to figure out which parantheses was missed. If the server.R or ui.R is more several hundred lines break the files into multiple smaller files and make sure you use functions as often as possible. Using functions also helps to follow the DRY principle (don't repeat yourself).

-   USE BITBUCKET. Preserve the integrity of your application by creating feature branches rather than merging into master. Checkout the [version control documents](http://tpldocs.rice.edu/display/PROG/Version+Control) on confluence for more information.

More resources
--------------

-   [R Studio](https://shiny.rstudio.com/)
-   [RStudio Community](https://community.rstudio.com/c/shiny)
