<?php
    $email = $password = "";
    $emailErr = $passwordErr = "";

    if ($_SERVER["REQUEST_METHOD"] == "POST") {
        if (empty($_POST["email"])) {
            $emailErr = "Email is required";
        } else {
            $email = test_input($_POST["email"]);
            if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
                $emailErr = "Invalid email format";
            }
        }

        if (empty($_POST["password"])) {
            $passwordErr = "Password is required";
        } else {
            $password = test_input($_POST["password"]);
        }
    }

    function test_input($data) {
        $data = trim($data);
        $data = stripslashes($data);
        $data = htmlspecialchars($data);
        return $data;
    }
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="shortcut icon" href="../assets/img/logo2.png" type="image/x-icon">
    <link rel="stylesheet" href="../assets/stylesheet/base.css" />
    <link rel="stylesheet" href="../assets/stylesheet/form.css">
    <style>
        h1 {
            font-size: 2rem;
        }
    </style>
    <title>Ratna Rajyalaxmi Campus</title>
</head>

<body>
    <div class="page">
        <nav>
            <div class="logoContainer">
                <a href="../index.php"><img class="logo" src="../assets/img/logo2.png" alt="logo" /></a>
                <span>RR Campus</span>
            </div>

            <div class="navlinks">
                <ul>
                    <li><a href="./about.php">About us</a></li>
                    <li>
                        <div class="dropdown">
                            <a>Programs ↓</a>
                            <div class="dropdown-content">
                                <a href="./bachelor.php">Bachelor</a>
                                <a href="./masters.php">Masters</a>
                            </div>
                        </div>
                    </li>
                    <li><a href="./admission.php">Admission</a></li>
                    <li><a href="./calender.php">Calender</a></li>
                </ul>
            </div>

            <div>
                <a href="./student.php"><button class="btnMain">Student Portal</button></a>
                <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 50 50" width="30px" height="30px">
                    <path
                        d="M 5 8 A 2.0002 2.0002 0 1 0 5 12 L 45 12 A 2.0002 2.0002 0 1 0 45 8 L 5 8 z M 5 23 A 2.0002 2.0002 0 1 0 5 27 L 45 27 A 2.0002 2.0002 0 1 0 45 23 L 5 23 z M 5 38 A 2.0002 2.0002 0 1 0 5 42 L 45 42 A 2.0002 2.0002 0 1 0 45 38 L 5 38 z" />
                </svg>
            </div>
        </nav>

        <hr>

        <h1 class="txtcenter">Student Portal login</h1>
        <br><br>

        <div class="container">
            <main>
                <p class="formTitle">Email</p>
                <input size="40" class="simple" type="email" placeholder="name@site.com" name="mail">

                <br><br>

                <p class="formTitle">Password</p>
                <input size="40" class="simple" placeholder="***********" type="password" name="password">

                <br><br>

                <input class="submit" type="submit" value="Login">
            </main>
        </div>

    </div>

    <footer>
        <div class="footer-main">
            <div>
                <img class="logo" src="../assets/img/logo2.png" alt="logo" />
                <p>
                    <b> Ratna Rajyalaxmi Campus, </b><br />
                    Tribhuvan University
                </p>
            </div>
            <div>
                <h4><b>Explore</b></h4>
                <ul>
                    <li><a href="#">News</a></li>
                    <li><a href="#">Admissions</a></li>
                    <li><a href="#">Library</a></li>
                    <li><a href="#">About us</a></li>
                    <li><a href="#">Publications</a></li>
                </ul>
            </div>

            <div>
                <h4><b>University Links</b></h4>
                <ul>
                    <li><a href="https://tuexam.edu.np/">University Exams</a></li>
                    <li><a href="https://fohss.tu.edu.np/">FoHSS</a></li>
                    <li>
                        <a href="https://www.ugcnepal.edu.np/">University grants commission</a>
                    </li>
                    <li>
                        <a href="https://moest.gov.np/">Ministry of Education, Science, & Technology</a>
                    </li>
                </ul>
            </div>

            <div>
                <h4><b>Address</b></h4>
                <p>
                    Visit:
                    <a href="https://www.google.com/maps/place/Ratna+Rajyalaxmi+Campus/@27.7028552,85.3173056,17z">Pradarshani
                        Marg, Kathmandu</a>
                    <br />
                    Phone:
                    <a href="tel:015325819">015325819</a>
                    <br />
                    Mail:
                    <a href="mailto:info@rrlc.tu.edu.np">info@rrlc.tu.edu.np</a>
                </p>
            </div>
        </div>
        <div class="footer-extended">
            <p>© 2024 Ratna Rajyalaxmi Campus, Tribhuvan University</p>
        </div>
    </footer>
</body>

</html>
