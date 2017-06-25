
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
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>images/Logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<!-- Stacomi Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="images/stacomi_logo.png" border="0" alt="Stacomi Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p>Welcome to StacomiR - Fish Migration Monitoring</p>

<blockquote>stacomiR stands for migratory control stations. It enables to read informations in a database dedicated to the monitoring of fish migrations.It allows to build overviews of fish migrations and create charts with data.</blockquote>

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

<p><img src="images/stacomi.png" alt="Stacomi"></p>

<p> The <strong>project summary page</strong> is <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
