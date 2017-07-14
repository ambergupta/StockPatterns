# StockPatterns
After observing stock market prices for quite some time now , I had come to conclusion that stock prices do form certain patterns again and again as dicovered by many earlier. There is no denying this fact. Finding causation for pattern formation may lead one to very misleading path. Patterns are order created in stock prices chaos. When I stated looking for any tools available to detect patterns , I did not find anything in public domain that match my expectation . Therefore after some thinking I figured that data analytics / ML algos can be used for this purpose. Therefore after getting through formal anaytics education , I have created this working project to detect certain patterns on various time scales for Indian stock markets. I have opened all code without comments and data aquisition code(Not shared) is my properity tool. Brief of code files coming in a day or two

Features.R          : Contain code to convert patterns into features

ModelBuilding.R     : Create core dataset on which model is built and persisted

SyntheticDataGen.R  : Utility to generate synthetic patterns . Handy for testing new patterns

livedata.R          : Perform pattern detecting on real data using model built earlier

timescaling.R       : Experimental file 

