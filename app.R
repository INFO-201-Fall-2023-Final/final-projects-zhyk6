library(shiny)
library(ggplot2)
library(dplyr)

# RUN THE data_wrangling.R FILE TO GET THE CLEANED DATAFRAMES FIRST
source("data_wrangling.R")


ui <- fluidPage(
  titlePanel("YouTube Videos Analysis"),
  tabsetPanel(
    tabPanel("Scrollytelling",
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   id = "intro",
                   h2("Story Pitch"),
                   p("YouTube has become a staple in the world of online video sharing, and has grown to become the largest platform for video content. According to the statistics, YouTube has 2.6 billion active monthly users and around half of the internet users in the world have access to YouTube. For a world trending video platform, it has already made extraordinary contributions to connect people together. YouTubers can upload videos and they can range from individuals filming themselves with their smartphones to large media companies producing high-quality
content. With the abundance of videos being shared and watched on this platform, it is important to analyze the trends and preferences of the audience as well as the YouTubers to gain insight into the evolving online video consumption landscape. By analyzing both the audience
preferences and the YouTubers, we could give suggestions like what steps you should take to become a successful YouTuber."),
                   p("Scroll down to explore insights from our datasets.")
                 ),
                 tags$div(
                   id = "section1",
                   h2("Section 1: Views and Engagement"),
                   # Include the bubble chart
                   plotOutput("bubble_chart"),
                   verbatimTextOutput("bubble_description"),
                   dataTableOutput("engagement_table")
                 ),
                 tags$div(
                   id = "section2",
                   h2("Section 2: Video Categories"),
                   # Include the bar charts
                   plotOutput("channel_category"),
                   verbatimTextOutput("channel_category_description"),
                   plotOutput("category_counts"),
                   verbatimTextOutput("category_counts_description"),
                   plotOutput("category_views"),
                   verbatimTextOutput("category_views_description")
                 ),
                 tags$div(
                   id = "section3",
                   h2("Section 3: Box Plot by Category"),
                   plotOutput("box_plot"),
                   verbatimTextOutput("box_plot_description")  # Description for box plot
                 ),
                 
                 tags$div(
                   id = "conclusion",
                   h2("Conclusion"),
                   p("These were some key insights from our analysis."),
                   p("For more details, refer to the summary page.")
                 )
               )
             )
    ),
    tabPanel("Summary Takeaways & About Page",
             fluidRow(
               column(
                 width = 12,
                 h2("Summary Takeaways"),
                 p("Here are the key findings and summarise table from our analysis: Some most popular categories on YouTube
                   are Gaming, Music Videos, Vlogs and Lifestyle, Educational Content, Comedy and Sketches,
                   Tech Reviews, Animation, Beauty and Fashion and DIY and How-To etc. The popularity of 
                   YouTube channels can vary by region, age group, and personal preferences. For the latest
                   trends and popular channels, you may want to check recent sources and platforms that specialize
                   in tracking YouTube analytics and trends."),
                 # Include summary tables, key insights, etc.
                 dataTableOutput("summary_table")
               ),
               column(
                 width = 12,
                 h2("About"),
                 p("This analysis was conducted using datasets obtained from:"),
                 p("https://www.kaggle.com/datasets/datasnaek/youtube-new?select=USvideos.csv"),
                 p("https://www.kaggle.com/datasets/themrityunjaypathak/most-subscribed-1000-youtube-channels"),
                 p("Authors: Yukang Zhao")
               )
             )
    )
  )
)


server <- function(input, output) {
  # Bubble chart for Section 1
  output$bubble_chart <- renderPlot({
    mean_data <- aggregate(cbind(Like_View_Ratio, views, days_from_pub_to_trend) ~ title, data = us_video, FUN = mean)
    
    ggplot(mean_data, aes(x = days_from_pub_to_trend, y = Like_View_Ratio, size = views, color = title)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 10)) +
      labs(title = "Bubble Chart: Views vs Like_View_Ratio by Category",
           x = "Average Days From Publish to Trending", y = "Like-View Ratio", size = "Total Views", color = "Category") +
      theme_minimal() +
      guides(color = guide_legend(title = "Category"))
  })
  
  # Bar chart 1: Counts of videos by category for Section 2
  output$category_counts <- renderPlot({
    count_data <- us_video %>%
      group_by(title) %>%
      summarise(count = n())
    
    ggplot(count_data, aes(x = title, y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Count of Videos by Category",
           x = "Category", y = "Total Counts") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Bar chart 3: top 100 subscribed channel categories.
  output$channel_category <- renderPlot({
    category_data <- top_1000 %>%
      group_by(Category) %>%
      summarise(Subscribers = n())
    
    ggplot(category_data, aes(x = Category, y = Subscribers)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Categories of channel on top 1000 subscribe",
           x = "Category", y = "Number of Channels") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$bubble_description <- renderText({
    "This bubble chart displays the relationship between total views, Like View Ratio, and categories of videos in the US.

By looking at the graph above, we find that the category Nonprofits & Activism have highest views and 
like-view ratio, and shortest time from publish to trending. Followed by the Music category, which has 
the second highest statistics in both mean like-view ratio and views, so if one want to start a YouTube
channel, they probably want to consider a Music channel.
    
The average like-view ratio for all videos in the US is around 0.0343, which Comedy, Education, Gaming, 
Howto & Style, Music, Nonprofits & Activism, and People & Blogs have higher ratio than the average. Hence,
those might be somewhere a new YouTuber want to start with, since videos in those categories tend to have
higher rate of user interactions than the others, implies those categories are more popular."
  })
  
  output$category_counts_description <- renderText({
    "This bar chart represents the count of videos in each category in the US.

In general, we suggest a new YouTuber consider a category with less total videos. Since the more videos in
a category, implies the more YouTubers who make videos of this type, and the fiercer the competition.
On the contrary, if there are fewer videos in a category, the competition in this category will be less fierce,
which will be more friendly to novices."
  })
  
  output$channel_category_description <- renderText({
    "This bar chart represents the channel of videos on top 1000 in YOUTUBE.

With this bar people can realize what categories of channel are more popular intuitively. People can
compare the number of subscribers between channels with different tags at the same time."
    
  })
  
  output$category_views_description <- renderText({
    "This bar chart shows the average views for each category.
    
The bar chart is more obvious than the bubble chart at the beginning. In general, the higher the average views,
the more popular a category should be."
  })
  
  # Bar chart 2: Average views by category for Section 2
  output$category_views <- renderPlot({
    views_data <- us_video %>%
      group_by(title) %>%
      summarise(avg_views = mean(views))
    
    ggplot(views_data, aes(x = title, y = avg_views)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      labs(title = "Average Views by Category",
           x = "Category", y = "Average Views") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # box plot
  output$box_plot <- renderPlot({
    ggplot(us_video[us_video$days_from_pub_to_trend <= 20, ], aes(x = title, y = days_from_pub_to_trend)) +
      geom_boxplot(fill = "skyblue") +
      labs(x = "Category", y = "Days from Publish to Trending", 
           title = "Box Plot of Days from Publish to Trending by Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Description for the box plot
  output$box_plot_description <- renderText({
    "This box plot displays the distribution of days from publish to trending for each video category.

From the box chart we can see that the average days from publish to trending is around 5 days, except for Shows,
which can be interpret by its special feature of different update frequency than the others. In general, we find
it is more likely for a video to get trending after a short period of time after its publish, and the probability
of getting trending is decreasing throughout time. This fact tells us that a YouTuber should probably try to make
their cover and title of the videos as eye-catching as possible, in order to increase the chance for the videos to
be clicked on and getting trending quicker.
    
Notice we ignored videos that got trending more than 20 days after its publish, which contains less than 3% of our data,
those videos can be seen as outliers and also support our claim, but to make the box chart more readable, we choose to
drop those videos from our data."
  })
  
  # Summary table for Summary Takeaways & About Page
  output$summary_table <- renderDataTable({
    mean_data <- us_video %>%
      group_by(title) %>%
      summarise(mean_views = mean(views),
                mean_like_view_ratio = mean(Like_View_Ratio, na.rm = TRUE),
                mean_day_from_pub_to_trend = mean(days_from_pub_to_trend),
                mean_num_comments = mean(comment_count))
    mean_data
  })
}


shinyApp(ui = ui, server = server)
