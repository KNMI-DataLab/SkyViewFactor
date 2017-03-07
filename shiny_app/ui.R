
pageWithSidebar(
headerPanel('Settings for the SVF'),
sidebarPanel(
sliderInput("pointX","X-coordinate",min=120874,max=160874,value=140874,step=100),

sliderInput("pointY","Y-coordinate",min=437916,max=477916,value=457916,step=100),

sliderInput("xres","xres",min=1,max=15,value=5,step=1),

sliderInput("yres","yres",min=1,max=15,value=5,step=1),

sliderInput("maxView","Radius",min=100,max=500,value=100,step=50),

sliderInput("angles","angles",min=4,max=32,value=16,step=4)
),
mainPanel(
  plotOutput('plot1')
)
)