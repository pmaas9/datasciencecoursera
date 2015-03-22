shinyServer(
  function(input, output) {
    #output$response <- renderText({"hi"}) #how_much( number=user_number, years=user_duration, state=user_state, time=user_time_units, pack=user_units )
    
    output$response <- renderText({
    
    if((is.numeric(input$user_number)==0)+(is.numeric(input$user_duration)==0)>0){
      response = "Please input numbers for quantity smoked and number of years smoking."      
    }
    else if( (input$user_number<=0) + (input$user_duration<=0) >0){
      response = "Please input numbers that are greater than 0."
    }
    else if( (input$user_number>10000) + (input$user_duration>10000) >0){
      response = "Please use reasonable numbers for quantity smoked and number of years smoking."      
    }else{
    #if(input$user_units == "cigarettes"){use_units = 0};
    #if(input$user_units == "packs"){use_units = 1};
    if(input$user_units == 1){use_units = 0};
    if(input$user_units == 2){use_units = 1};
    if(input$user_time_units == 1){use_time_units = "day"};
    if(input$user_time_units == 2){use_time_units = "week"};
    
    response = how_much( number=input$user_number, years=input$user_duration, state=input$user_state, time=use_time_units, pack=use_units )
    }
    response
    }) 
   }
)

#number=1, years=1, state="Missouri", time="day", pack=1