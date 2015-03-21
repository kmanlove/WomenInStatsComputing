install.packages("rvest")
# install.packages("~/Downloads/rvest_0.1.0.tar.gz", repos = NULL)
install.packages("rlist")
install.packages("pipeR")

require(rvest)
require(rlist)
require(pipeR)

# from http://renkun.me/blog/2014/07/29/scraping-information-of-cran-packages.html

# first, set urls
url <- "http://cran.r-project.org/web/packages/available_packages_by_date.html"
pkgurl <- "http://cran.r-project.org/web/packages/%s/index.html"
page <- html(url)


# set a start time so that we know how long it takes later
start <- Sys.time()

# start scraping!
data <- page %>>%
#  html_node(xpath = "//tr") %>>%
  html_nodes(xpath = "//tr") %>>%
  #  html_node("//tr", xpath = TRUE) %>>%
  #  html_node("//tr") %>>%
  list.skip(1) %>>%
  # put list into k
  # look at k[[6421]]
  # k.1 <- k[[1]]
  # k.6421 <- k[[6421]]
  list.map(row ~
             # xpath tip: use "|" to select multiple nodes at the same time
             row %>>%
#              html_node(xpath = "//th[1]//text()") %>>%
#             html_node(x = k.6421, xpath = "//td[1]//text() | //td[2]//text() | //td[3]//text()") %>>%
             html_node(x = k.6421, xpath = "//td[1]//text() | //td[2]//a//text()") %>>%
            html_node(x = k.6421, xpath = "//td[2]//a//text()") %>>%
            html_node(xpath = "td[1]//text() | td[2]//text() | td[3]//text()") %>>%
  #              html_node(xpath = "tr[1]//text() | tr[2]//a//text() | tr[3]//text()") %>>%
              html_text(trim = TRUE) %>>%
             setNames(c("date", "package","title"))) %>>%
  
  # the table is ready, do some cleaning work
  list.update(date = as.Date(date)) %>>%
  
  # we only get the packages updated after July 28, 2014.
  list.filter(date >= as.Date("2014-07-28")) %>>%
  
  # scrape the individual page for each package
  list.update(html = {
    cat("[",format(Sys.time() - start,format="%s"),"]", .i,
        format(date,format = "%y-%m-%d"), package ,"\n", sep="\t")
    sprintf(pkgurl, package) %:>%
      content(GET(.),"parsed")
  }) %>>%
  
  # for each list member, html is the parsed document from which
  # we can extract detailed information
  # make good use of XPath: it can filter and select at the same time
  list.update(version = {
    page %>>%
      html_node("//tr[td[1]='Version:']//td[2]//text()", xpath = TRUE) %>>%
      html_text
  }, imports = {
    page %>>%
      html_node("//tr[td[1]='Imports:']//td[2]//a//text()", xpath = TRUE) %>>%
      html_text
  }, suggests = {
    page %>>%
      html_node("//tr[td[1]='Suggests:']//td[2]//a//text()", xpath = TRUE) %>>%
      html_text
  }) %>>%
  
  # remove html field
  list.update(html = NULL)


#-- My attempts --#
# need to get list of package names that get plugged into pkgurl where "decode" is. 
url <- "http://cran.r-project.org/web/packages/available_packages_by_date.html"
page <- html(url)
data <- page %>>%
  html_nodes(xpath = "//tr") %>>%
  html_table(header = NA) %>>%
  #  html_node("//tr", xpath = TRUE) %>>%
  #  html_node("//tr") %>>%
  list.skip(1) %>>% 
  
data2 <- xmlToList(data)

  list.map(row ~
             # xpath tip: use "|" to select multiple nodes at the same time
             row %>>%
             #              html_node(xpath = "//th[1]//text()") %>>%
             #             html_node(x = k.6421, xpath = "//td[1]//text() | //td[2]//text() | //td[3]//text()") %>>%
             html_nodes(xpath = "//td[2]//a//text()"))

# index.urls <- vector("list", length(data))
# for(i in 1:length(data)){
#   data.in <- data[[i]]
#   index.urls[[i]] <- html_node(x = data.in, xpath = "//tr[i]//td[2]//a//text()")
# }
#   # put list into k
#   # look at k[[6421]]
#   # k.1 <- k[[1]]
#   # k.6421 <- k[[6421]]
#   list.map(row ~
#              # xpath tip: use "|" to select multiple nodes at the same time
#              row %>>%
#              #              html_node(xpath = "//th[1]//text()") %>>%
#              #             html_node(x = k.6421, xpath = "//td[1]//text() | //td[2]//text() | //td[3]//text()") %>>%
#              html_node(x = k.6421, xpath = "//td[1]//text() | //td[2]//a//text()") %>>%
             


# code to get into each package's index page to pull author info
pkgurl <- "http://cran.r-project.org/web/packages/decode/index.html"
page.pkg <- html(pkgurl)

data <- page.pkg %>>%
  #  html_node(xpath = "//tr") %>>%
  html_nodes(xpath = "//tr") %>>%
  html_node(xpath = "//tr[5]//td[2]//text()")

  #  html_node("//tr", xpath = TRUE) %>>%
  #  html_node("//tr") %>>%
#  list.skip(3) %>>%
  # put list into decode.ind
  # decode.ind.1 <- decode.ind[[1]]
  list.map(row ~
             # xpath tip: use "|" to select multiple nodes at the same time
             row %>>%
#             html_node(xpath = "//tr[5]//td[2]//text()")
           html_node(x = decode.ind, xpath = "//tr[5]//td[2]//text()")
           html_text(trim = TRUE)) %>>%
             setNames("author")) %>>%
  