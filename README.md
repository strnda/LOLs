# RFun
silly stuff doable in R (or collected works of my procrastination periods)

> devtools::install_github('strnda/RFun') 
> 
> library(RFun)
> 
> secRet('Fíla') +    
>   labs(x = '', y = '') +    
>   theme(axis.ticks = element_blank(),    
>         axis.text = element_blank())
> 
> logo(name = 'L175', cols = c('#008080', '#ceffff'), include_title = F, additional_text = c('Adélka, Fíla, Venca, Vočko, Vojtí')) +    
>   theme(panel.grid.major = element_blank(),    
>         panel.grid.minor = element_blank(),    
>         axis.ticks = element_blank(),    
>         axis.text = element_blank(),    
>         axis.line = element_blank(),    
>         plot.title = element_text(size = 25, hjust = 0.5))    
> 
> eveRloving('Phill + Caroline', 200, 100, '#1f3b51', '#aac6dc')
> 
> mondRian() + theme_void()
> 
> smiley(T)
