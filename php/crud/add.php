<html>

<head>
	<link rel="stylesheet" href="./styles/base.css">
	<link rel="stylesheet" href="./styles/form.css">
	<title>Add Data</title>
</head>

<body>
	<h2>Add Data</h2>
	<br>
	<p>
		<a href="home.php">← Go back</a>
	</p>

	<form action="addAction.php" method="post" name="add">
		<div class="grid">
			<div>
				<p>Name</p>
				<input class="simple" size="40" type="text" name="name">

				<p>Email</p>
				<input class="simple" size="40" type="text" name="email">
				<br><br>
			</div>
		</div>
		<input class="submit" type="submit" name="submit" value="Add">
	</form>
</body>

</html>