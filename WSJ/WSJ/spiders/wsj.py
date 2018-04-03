# -*- coding: utf-8 -*-
import scrapy
from scrapy.loader import ItemLoader
from WSJ.items import WsjItem

def getCleanStartUrlList(filename):
    myfile = open(filename, "r")
    urls = myfile.readlines()
    #print 'ok1'
    first = [url.strip() for url in urls]
    #print len(first)    
    return first

class WsjSpider(scrapy.Spider):
    name = 'wsj'
    allowed_domains = ['www.wsj.com']

    url_list = getCleanStartUrlList('/Users/sijinlin/Desktop/wsj.txt')
    start_urls = url_list 
    #start_urls = (
        #'http://www.wsj.com/articles/SB10001424052970203462304577134474072180022',
        #)

    def parse(self, response):
        l = ItemLoader(item=WsjItem(), response=response)
    	
        title = response.xpath('//*[@class="wsj-article-headline"]/text()').extract()
        cat = response.xpath('//*[@class="flashline-category"]/text()').extract_first()
        category = [cat.strip()]
        date = response.xpath('//*[@class="timestamp"]/text()').extract()

        l.add_value('title', title)
        l.add_value('category', category)
        l.add_value('date', date)
        
   
        return l.load_item()
