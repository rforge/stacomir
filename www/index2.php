<!doctype html>
<!--
	Material Design Lite
	Copyright 2015 Google Inc. All rights reserved.
	
	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at
	
	https://www.apache.org/licenses/LICENSE-2.0
	
	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License
-->
<html lang="en">
	<head>
		<meta charset="utf-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge">
		<meta name="description" content="A front-end template that helps you build fast, modern mobile web apps.">
		<meta name="viewport" content="width=device-width, initial-scale=1.0, minimum-scale=1.0">
		<title>StacomiR package homepage</title>
		
		<link rel="shortcut icon" href="images/favicon.png">
		
		<!-- SEO: If your mobile URL is different from the desktop URL, add a canonical link to the desktop page https://developers.google.com/webmasters/smartphone-sites/feature-phones -->
		<!--
			<link rel="canonical" href="http://www.example.com/">
		-->
		
		<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Roboto:regular,bold,italic,thin,light,bolditalic,black,medium&amp;lang=en">
		<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
		<link rel="stylesheet" href="$$hosted_libs_prefix$$/$$version$$/material.cyan-light_blue.min.css">
		<link rel="stylesheet" href="styles.css">
		
		<!-- Style Material Design -->
		<link rel="stylesheet" href="https://storage.googleapis.com/code.getmdl.io/1.0.6/material.indigo-pink.min.css">
		<script src="https://storage.googleapis.com/code.getmdl.io/1.0.6/material.min.js"></script>
		<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
		<style>
			#view-source {
			position: fixed;
			display: block;
			right: 0;
			bottom: 0;
			margin-right: 40px;
			margin-bottom: 40px;
			z-index: 900;
			}
		</style>
	</head>
	<body>
		<div class="demo-layout mdl-layout mdl-js-layout mdl-layout--fixed-drawer mdl-layout--fixed-header">
			<header class="demo-header mdl-layout__header mdl-color--grey-100 mdl-color-text--grey-600">
				<div class="mdl-layout__header-row">
					<span class="mdl-layout-title">StacomiR Project</span>
					<div class="mdl-layout-spacer"></div>
					<button class="mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--icon" id="hdrbtn">
						<i class="material-icons">more_vert</i>
					</button>
					<ul class="mdl-menu mdl-js-menu mdl-js-ripple-effect mdl-menu--bottom-right" for="hdrbtn">
						<li class="mdl-menu__item">About</li>
						<li class="mdl-menu__item">Contact</li>
					</ul>
				</div>
			</header>
			<div class="demo-drawer mdl-layout__drawer mdl-color--blue-grey-900 mdl-color-text--blue-grey-50">
				<header class="demo-drawer-header">
					<img src="images/icon.png" class="demo-avatar">
					<div class="demo-avatar-dropdown">
						<span>stacomiR</span>
						<div class="mdl-layout-spacer"></div>
						<button id="accbtn" class="mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--icon">
							<i class="material-icons" role="presentation">arrow_drop_down</i>
							<span class="visuallyhidden">Packages CRAN</span>
						</button>
						<ul class="mdl-menu mdl-menu--bottom-right mdl-js-menu mdl-js-ripple-effect" for="accbtn">
							<li class="mdl-menu__item"><a href="https://cran.r-project.org/package=stacomirtools">stacomirtools</a></li>
						</ul>
					</div>
				</header>
				<nav class="demo-navigation mdl-navigation mdl-color--blue-grey-800">
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/projects/stacomir/"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">home</i>R-forge page</a>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/scm/?group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">inbox</i>SCM</a>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/forum/?group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">forum</i>Forums</a>
					<a class="mdl-navigation__link" href=""><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">flag</i>Updates</a>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/project/memberlist.php?group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">people</i>Members</a>
					<div class="mdl-layout-spacer"></div>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/forum/forum.php?forum_id=3265&group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">help_outline</i><span class="visuallyhidden">Help forum</span></a>
				</nav>
			</div>
			<main class="mdl-layout__content mdl-color--grey-100">
				<div class="mdl-grid demo-content">
					<div class=" mdl-color--white mdl-shadow--2dp mdl-cell mdl-cell--12-col mdl-grid">
						<div class="mdl-card__supporting-text mdl-color-text--grey-600">
							<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
								$contents = '';
								while (!feof($handle)) {
									$contents .= fread($handle, 8192);
								}
								fclose($handle);
							echo $contents; } ?>
						</div>
					</div>
					<div class="mdl-shadow--2dp mdl-color--white mdl-cell mdl-cell--8-col">
						<div class="mdl-card__supporting-text mdl-color-text--grey-600">
							<p>Migratory fishes move upstream or downsteam in the rivers. Monitoring stations, often located on fishways installed on dams, provide counts of migrants, and those are usefull to stock management.</p>
							<p>The objective of the STACOMI project is to provide a database along with data treatment and a tool to import or type down fishes number.  The stacomiR package offers standardized treatments for migration monitoring stations</p>
							
							<ul>
								<li>Migration overview for one or several species, and stages.</li>
								<li>Migration analysed with environmental conditions</li>
								<li>Weight / number conversion for glass eel</li>
								<li>Silver eel migration analysis, along with maturation analysis</li>
								<li>Size-age conversion for salmon</li>
								<li>Analysis of fish characteristics (size, weight, sex…)</li>
								<li>Crossed analysis of quantitative and/or qualitative characteristics and migration</li>
								<li>Seasonality analysis, comparison of the annual migration with historical values</li>
								<li>Fishway and counting device operation</li>
							</ul>
							
							<p>To work with stacomi, you’ll need a database, and an installation of the program and java interface. Please contact the authors if you are interested in the project.<p>
								
								<p>Cédric Briand EPTB Vilaine <a href="mailto:cedric.briand@eptb-vilaine.fr">cedric.briand@eptb-vilaine.fr</a><br/>
									Timothée Besse LOGRAMI <a href="mailto:timothee.besse@logrami.fr">timothee.besse@logrami.fr</a><br/>
								Marion Legrand LOGRAMI <a href="mailto:marion.legrand@logrami.fr">marion.legrand@logrami.fr</a></p>
								<img src="images/stacomi.png" style="max-width: 100%;">
							</div>
							</div>
							<div class="demo-cards mdl-cell mdl-cell--4-col mdl-cell--8-col-tablet mdl-grid mdl-grid--no-spacing">
								<div class="demo-updates mdl-card mdl-shadow--2dp mdl-cell mdl-cell--4-col mdl-cell--4-col-tablet mdl-cell--12-col-desktop">
									<?PHP
										include('rssclass.php');
										$feedlist = new rss('http://www.migrateurs-loire.fr/feed/');
										echo $feedlist->display(9,"TRAC RSS");
									?>
									<div class="mdl-card__actions mdl-card--border">
										<a href="#" class="mdl-button mdl-js-button mdl-js-ripple-effect">Open TRAC</a>
									</div>
								</div>
								
							</div>
						</div>
					</main>
				</div>
				<script src="$$hosted_libs_prefix$$/$$version$$/material.min.js"></script>
			</body>
		</html>
		