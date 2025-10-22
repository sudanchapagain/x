<?php
require_once("dbConnection.php");

if (isset($_POST['update'])) {
	$id = mysqli_real_escape_string($mysqli, $_POST['id']);
	$name = mysqli_real_escape_string($mysqli, $_POST['name']);
	$email = mysqli_real_escape_string($mysqli, $_POST['email']);	
	
	if (empty($name) || empty($email)) {
		if (empty($name)) {
			echo "<font color='red'>Name field is empty.</font><br/>";
		}
		
		if (empty($email)) {
			echo "<font color='red'>Email field is empty.</font><br/>";
		}
	} else {
		$result = mysqli_query($mysqli, "UPDATE users SET `name` = '$name', `email` = '$email' WHERE `id` = $id");
		
		echo "<p><font color='green'>Data updated successfully!</p>";
		echo "<a href='home.php'>View Result</a>";
	}
}
