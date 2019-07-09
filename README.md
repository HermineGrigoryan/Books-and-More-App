# Books & More - Group Project

The link to the shiny app can be found [here](https://hermine.shinyapps.io/Books-and-More/)

The "Books & More" application aims to provide statistical information, qualitative and
quantitative analysis targeted not only towards people with a desire to find an interesting book
but also towards book analysts looking for reliable and accurate information. The application
itself is a source of information and data collection about the variety of books and authors stored
within its library.

The data used for the application has been retrieved from the websites **amazon.com** and
**goodreads.com**. Due to the very large amount of the necessary data for the application, other
different methods have been used in order to store the data, such as attaching databases to the
application and, also manually collecting and formatting the data. In order to perform the
analysis, which were done over the entire texts of the books, we have downloaded the data from
the internet in PDF format. As the analyses is based on the contexts of each book, a separate
database has been created where the texts of each book have been stored and later attached to the
application in order to improve the quality of the data and to ensure its ease of use and
effectiveness. Navicat, a database management and development software, for Postresql has been
used. We have created a table within the database where we have inserted all of the records of
the books and then exported them to a CSV file.

To make it easier for the user to understand the data and not spend much time trying to read
columns of statistical information, the data is being presented in the form of visual
demonstrations. The application contains over 200 plots. Most of the plots are interactive and
responsive to the user’s inputs. For the plots which are based on the texts of the books pictures
have been inserted since most of the books are 200+ pages and it would have been very time
consuming and not user friendly to have the plots go over the entire texts of the books to plot
each graph.

