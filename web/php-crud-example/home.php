<?php
require_once("dbConnection.php");

$result = mysqli_query($mysqli, "SELECT * FROM users ORDER BY id DESC");
?>

<html>
<head>
		<link rel="stylesheet" href="./styles/base.css">
	<link rel="stylesheet" href="./styles/form.css">
	
	<title>Homepage</title>
</head>

<body>
	<h2>Dashboard</h2>
	<br>
	<p>
		<a href="add.php"><button>Add New Data</button></a>
	</p>
	<table width='80%' border=0>
		<tr bgcolor='#eee'>
			<td><strong>Name</strong></td>
			<td><strong>Email</strong></td>
			<td><strong>Action</strong></td>
		</tr>
		<?php
		while ($res = mysqli_fetch_assoc($result)) {
			echo "<tr>";
			echo "<td>".$res['name']."</td>";
			echo "<td>".$res['email']."</td>";	
			echo "<td><a href=\"edit.php?id=$res[id]\">Edit</a> | 
			<a href=\"delete.php?id=$res[id]\" onClick=\"return confirm('Are you sure you want to delete?')\">Delete</a></td>";
		}
		?>
	</table>

	<style>
		tr > td {
				padding: 10px;
		}
	</style>

</body>
</html>
