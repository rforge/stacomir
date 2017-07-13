
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php
	
	$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
	$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
	$themeroot='http://r-forge.r-project.org/themes/rforge/';
	
	echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">
	
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
		<title><?php echo $group_name; ?></title>
		<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
		<meta name="viewport" content="width=device-width, initial-scale=1.0, minimum-scale=1.0">
		
		<!-- Style Material Design -->
		<link rel="stylesheet" href="https://storage.googleapis.com/code.getmdl.io/1.0.6/material.indigo-pink.min.css">
		<script src="https://storage.googleapis.com/code.getmdl.io/1.0.6/material.min.js"></script>
		<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Roboto:regular,bold,italic,thin,light,bolditalic,black,medium&amp;lang=en">
		<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
		<link rel="stylesheet" href="https://code.getmdl.io/1.3.0/material.blue_grey-blue.min.css" />
		<link rel="stylesheet" href="styles.css">
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
					<span class="mdl-layout-title">Fish Migration Monitoring (stacomiR) project</span>
					<div class="mdl-layout-spacer"></div>
					<button class="mdl-button mdl-js-button mdl-js-ripple-effect mdl-button--icon" id="hdrbtn">
						<i class="material-icons">more_vert</i>
					</button>
					<ul class="mdl-menu mdl-js-menu mdl-js-ripple-effect mdl-menu--bottom-right" for="hdrbtn">
						<li class="mdl-menu__item"><a href="http://www.eptb-vilaine.fr/site/index.php/les-migrateurs/stacomi/presentation">About</a></li>
						<li class="mdl-menu__item"><a href="mailto:cedric.briand@eptb-vilaine.fr"  target="_blank">Contact</a></li>
					</ul>
				</div>
			</header>
			<div class="demo-drawer mdl-layout__drawer mdl-color--blue-grey-50 mdl-color-text--blue-grey-900">
				<header class="demo-drawer-header">
					<img src="images/stacomi_logo.png">
				</header>
				<nav class="demo-navigation mdl-navigation mdl-color--blue-grey-800">
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/projects/stacomir/"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">home</i>R-forge page</a>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/R/?group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">archive</i>R packages</a>
					<a class="mdl-navigation__link" href="https://www.rdocumentation.org/packages/stacomiR/" target="_blank"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">archive</i>Documentation</a>
					<a class="mdl-navigation__link" href="https://r-forge.r-project.org/scm/?group_id=1019"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">autorenew</i>SCM</a>
					<a class="mdl-navigation__link" href="https://groups.google.com/forum/#!forum/stacomi"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">forum</i>Google group</a>
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
							<p>stacomiR stands for migratory control stations. It enables to read informations in a database dedicated to the monitoring of fish migrations.It allows to build overviews of fish migrations and create charts with data.</p>
							<img src="images/stacomi_logo.png" width="150" style="max-width: 100%;"> 
							<img src="images/Logo.png" width="150" style="max-width: 100%;"> 
							<img src="images/logo_AFB.png" width="200" style="max-width: 100%;"> 
						</div>
					</div>
					<div class="mdl-shadow--2dp mdl-color--white mdl-cell mdl-cell--8-col">
						<div class="mdl-card__supporting-text mdl-color-text--grey-600">
							<p>Migratory fishes move upstream or downsteam in the rivers. Monitoring stations, often located on fishways installed on dams, provide counts of migrants, and those are usefull to stock management.</p>
							<p>The objective of the STACOMI project is to provide a database along with data treatment and a tool to import or type down fishes number.  The stacomiR package offers standardized treatments for migration monitoring stations</p>
							
							<ul class="mdl-list">
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">compare_arrows</i> Migration overview for one or several species, and stages.</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">multiline_chart</i> Migration analysed with environmental conditions</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">line_weight</i> Weight / number conversion for glass eel</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">show_chart</i> Silver eel migration analysis, along with maturation analysis</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">straighten</i> Size-age conversion for salmon</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">insert_chart</i> Analysis of fish characteristics (size, weight, sex…)</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">functions</i> Crossed analysis of quantitative and/or qualitative characteristics and migration</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">timeline</i> Seasonality analysis, comparison of the annual migration with historical values</span></li>
								<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><i class=" mdl-list__item-icon material-icons" role="presentation">switch_video</i> Fishway and counting device operation</span></li>
							</ul>
							
							<p>To work with stacomi, you’ll need a database, and an installation of the program and java interface. Please contact the authors if you are interested in the project.<p>
								
								<p>Cédric Briand EPTB Vilaine <a href="mailto:cedric.briand@eptb-vilaine.fr">cedric.briand@eptb-vilaine.fr</a><br/>
									Timothée Besse LOGRAMI <a href="mailto:timothee.besse@logrami.fr">timothee.besse@logrami.fr</a><br/>
								Marion Legrand LOGRAMI <a href="mailto:marion.legrand@logrami.fr">marion.legrand@logrami.fr</a></p>
								<img src="images/stacomi.png" style="max-width: 100%;">
							</div>
							</div>
							
							<div class="demo-cards mdl-cell mdl-cell--4-col mdl-cell--8-col-tablet mdl-grid mdl-grid--no-spacing">
								<!--<div class="demo-updates mdl-card mdl-shadow--2dp mdl-cell mdl-cell--4-col mdl-cell--4-col-tablet mdl-cell--12-col-desktop">
									<?PHP
										include('rssclass.php');
										$feedlist = new rss('https://r-forge.r-project.org/export/rss20_activity.php?group_id=1019');
										echo $feedlist->display(9,"Activity");
									?>
									<div class="mdl-card__actions mdl-card--border">
									<a href="https://r-forge.r-project.org/activity/?group_id=1019" class="mdl-button mdl-js-button mdl-js-ripple-effect">View activity</a>
									</div>
									</div>
								<div class="demo-separator mdl-cell--1-col"></div>-->
								<div class="demo-options mdl-card mdl-shadow--2dp mdl-cell mdl-cell--4-col mdl-cell--3-col-tablet mdl-cell--12-col-desktop">
									<div class="mdl-card__title mdl-card--expand mdl-color--blue-grey-400">
										<h2 class="mdl-card__title-text">Download packages</h2>
									</div>
									<div class="mdl-card__supporting-text mdl-color-text--grey-600">
										<h3><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">archive</i> StacomiR</h3>										
										Fish Migration Monitoring
										<ul class="mdl-list">
											<li class="mdl-list__item mdl-list__item--two-line"><span class="mdl-list__item-primary-content">
												<span><a href="https://r-forge.r-project.org/R/?group_id=1019">Latest</a></span>
											<span class="mdl-list__item-sub-title">R-forge</span></span>
											</li>
											<li class="mdl-list__item mdl-list__item--two-line"><span class="mdl-list__item-primary-content">
												<span><a href="http://cran.at.r-project.org/package=stacomiR" target="_blank">Stable</a></span>
												<span class="mdl-list__item-sub-title">CRAN</span>
											</span></li>
											<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><a href="https://cran.r-project.org/web/packages/stacomiR/stacomiR.pdf" target="_blank">Reference manual</a></span></li>
										</li>
									</ul>
									
									<h3><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">archive</i> Stacomirtools</h3>
									ODBC Connection Class for Package stacomiR
									<ul class="demo-list-two mdl-list">
										<li class="mdl-list__item mdl-list__item--two-line">
											<span class="mdl-list__item-primary-content">
												<span><a href="https://r-forge.r-project.org/R/?group_id=1019">Latest</a></span>
												<span class="mdl-list__item-sub-title">R-forge</span>
											</span>
										</li>
										<li class="mdl-list__item mdl-list__item--two-line"><span class="mdl-list__item-primary-content">
											<span><a href="http://cran.at.r-project.org/package=stacomirtools" target="_blank">Stable</a></span>
										<span class="mdl-list__item-sub-title">CRAN</span></span></li>
										<li class="mdl-list__item"><span class="mdl-list__item-primary-content"><a href="https://cran.r-project.org/web/packages/stacomirtools/stacomirtools.pdf" target="_blank">Reference manual</a></span></li>
									</ul>
								</div>
							</div>
							<div class="demo-separator mdl-cell--1-col"></div>
							<div class="demo-options mdl-card mdl-color--blue-grey-800 mdl-shadow--2dp mdl-cell mdl-cell--4-col mdl-cell--3-col-tablet mdl-cell--12-col-desktop">
								<div class="mdl-card__supporting-text mdl-color-text--blue-grey-50">
									<h3>Projects using StacomiR</h3>
									<ul class="mdl-list">
										<li class="mdl-list__item">
											<span class="mdl-list__item-primary-content"><a href="http://www.eptb-vilaine.fr/site/telechargement/migrateurs/Pechelec_2014-2015.pdf" target="_blank"><i class="material-icons">description</i>Briand, Sauvaget, Eriau 2016</a></span>
											<li class="mdl-list__item">
												<span class="mdl-list__item-primary-content"><a href="mailto:cedric.briand@eptb-vilaine.fr" target="_blank"><i class="material-icons">note_add</i>I'm using it</a></span>
											</li>
										</li>
									</ul>
								</div>
							</div>								
						</div>
					</div>
				</main>
			</div>
			<script src="$$hosted_libs_prefix$$/$$version$$/material.min.js"></script>
		</body>
	</html>
