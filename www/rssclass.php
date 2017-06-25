<?php
	class rss {
		var $feed;
		
		function rss($feed) 
		{   $this->feed = $feed;  }
		
		function parse() 
		{
			$rss = simplexml_load_file($this->feed);
			
			$rss_split = array();
			foreach ($rss->channel->item as $item) {
				$title = (string) $item->title; // Title
				$link   = (string) $item->link; // Url Link
				$description = (string) $item->description; //Description
				$rss_split[] = '<li class="mdl-list__item">
				<span class="mdl-list__item-primary-content"><a href="'.$link.'" target="_blank" title="" >
				'.$title.' 
				</a></span>
				</li>
				';
			}
			return $rss_split;
		}
		function display($numrows,$head) 
		{
			$rss_split = $this->parse();
			
			$i = 0;
			$rss_data = '
			<div class="mdl-card__title mdl-card--expand mdl-color--teal-300">
			<h2 class="mdl-card__title-text">'.$head.'</h2>
			</div>
			<div class="mdl-card__supporting-text mdl-color-text--grey-600">
			<ul class="demo-list-item mdl-list">';
			while ( $i < $numrows ) 
			{
				$rss_data .= $rss_split[$i];
				$i++;
			}
			$trim = str_replace('', '',$this->feed);
			$user = str_replace('&lang=en-us&format=rss_200','',$trim);
			$rss_data.='</ul></div>';
			return $rss_data;
		}
	}
?>