<?php
require_once("dbConnection.php");
$id = $_GET['id'];
$result = mysqli_query($mysqli, "SELECT * FROM users WHERE id = $id");
$resultData = mysqli_fetch_assoc($result);

$name = $resultData['name'];
$email = $resultData['email'];
?>
<html>
<head>
	<link rel="stylesheet" href="./styles/base.css">
	<link rel="stylesheet" href="./styles/form.css">
	<title>Edit Data</title>
</head>

<body>
    <h2>Edit Data</h2>
    <p>
	    <a href="home.php">Home</a>
    </p>
	
	<form name="edit" method="post" action="editAction.php">
				<p>Name</p>
				<input class="simple" type="text" name="name" value="<?php echo $name; ?>">

				<p>Email</p>
				<input class="simple" type="text" name="email" value="<?php echo $email; ?>">
			<br><br>
			<input type="hidden" name="id" value=<?php echo $id; ?>>
				<input class="simple" type="submit" name="update" value="Update">
		</form>
</body>
</html>
