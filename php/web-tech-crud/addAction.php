<html>
<head>
	<link rel="stylesheet" href="./styles/base.css">
	<link rel="stylesheet" href="./styles/form.css">
	<title>Add Data</title>
</head>

<body>
<?php
require_once("dbConnection.php");

if (isset($_POST['submit'])) {
	$name = mysqli_real_escape_string($mysqli, $_POST['name']);
	$email = mysqli_real_escape_string($mysqli, $_POST['email']);

	if (empty($name) || empty($email)) {
		if (empty($name)) {
			echo "<font color='red'>Name field is empty.</font><br/>";
		}
		
		if (empty($email)) {
			echo "<font color='red'>Email field is empty.</font><br/>";
		}
		echo "<br/><a href='javascript:self.history.back();'>Go Back</a>";
	} else { 
		$result = mysqli_query($mysqli, "INSERT INTO users (`name`, `email`) VALUES ('$name', '$email')");
		
		echo "<p><font color='green'>Data added successfully!</p>";
		echo "<a href='home.php'>View Result</a>";
	}
}
?>
</body>
</html>
