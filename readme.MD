## Making interactive maps with R - NICAR 2020

These files and folders are meant to accompany the hands-on [Making interactive maps with R](https://ireapps.github.io/nicar-2020-schedule#20200306_making_interactive_maps_with_r_2130_all) at 2020 NICAR in New Orleans. There's also a website walkthrough for folks who want to revisit what they learned in person and for those who couldn't attend.

### Downloading this Repo via R

The quickest way to access the data for this session is to first make sure your current project directory is logically organized. 

Hopefully, you've created folder on your computer in your `Documents` folder called `Projects` or something like that.

Once you've set your working directory (with maybe `setwd()`) then you can run these commands in your console to download the correct folders and data:

```
install.packages("usethis")
```

and then

```
usethis::use_course("https://github.com/andrewbtran/NICAR-2020-mapping/archive/master.zip")
```

----

Otherwise:

### Downloading this Repo by hand

Clone or download and unzip this repo. Make sure the folder is **not** in a temporary unzipped folder.

To ensure these files work as expected, please make sure the project directory is set up correctly: 

When you type `getwd()` it should look something like `your_file_path/NICAR-2020-mapping`

You can manually set this up by:

*Session > Set Working Directory > To Project Directory*

Alternatively, you can double click the **NICAR-2020-mapping.Rproj** file from your finder/file browser.

----

### Working with the files

Navigate to the `scripts` folder to open and follow along with each section by filename.


----


## License

The online version of this course is licensed under the [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

---

### [NICAR 2018 sessions](https://github.com/andrewbtran/NICAR-2019-mapping)
### [NICAR 2018 sessions](https://andrewbtran.github.io/NICAR/2018/)
### [NICAR 2017 sessions](https://andrewbtran.github.io/NICAR/2017/)