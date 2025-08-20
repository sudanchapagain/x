<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="shortcut icon" href="./assets/img/logo2.png" type="image/x-icon">
    <link rel="stylesheet" href="./style.css" />
    <title>Ratna Rajyalaxmi Campus</title>
</head>

<body>
<div class="page">
    <nav>
        <div class="logoContainer">
            <img class="logo" src="./assets/img/logo2.png" alt="logo" />
            <span>RR Campus</span>
        </div>

        <div class="navlinks">
            <ul>
                <li><a href="./pages/about.php">About us</a></li>
                <li>
                    <div class="dropdown">
                        <a>Programs ↓</a>
                        <div class="dropdown-content">
                            <a href="./pages/bachelor.php">Bachelor</a>
                            <a href="./pages/masters.php">Masters</a>
                        </div>
                    </div>
                </li>
                <li><a href="./pages/admission.php">Admission</a></li>
                <li><a href="./pages/calender.php"> Calender</a></li>
            </ul>
        </div>

        <div>
            <a href="./pages/student.php"><button class="btnMain">Student Portal</button></a>
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 50 50" width="30px" height="30px">
                <path
                    d="M 5 8 A 2.0002 2.0002 0 1 0 5 12 L 45 12 A 2.0002 2.0002 0 1 0 45 8 L 5 8 z M 5 23 A 2.0002 2.0002 0 1 0 5 27 L 45 27 A 2.0002 2.0002 0 1 0 45 23 L 5 23 z M 5 38 A 2.0002 2.0002 0 1 0 5 42 L 45 42 A 2.0002 2.0002 0 1 0 45 38 L 5 38 z" />
            </svg>
        </div>
    </nav>

    <hr />

    <main>
        <header>
            <div class="mainTxt">
                <h1>Ratna Rajyalaxmi Campus</h1>
                <p>
                    RR Campus is a constituent campus of Tribhuvan University & the
                    largest campus of faculty of humanities and social sciences. We
                    provide bachelor's and master's degree level programs.
                </p>

                <div class="headButtons">
                    <a href="#progs"><button class="btnPrimary">Explore programs</button></a>
                    <a href="./pages/about.php"><button class="btnSecondary">Learn more</button></a>
                </div>
            </div>

            <img class="mainImg" src="./assets/img/rr-1.jpg" alt="" />
        </header>

        <section class="about">
            <p class="titleSlim">About <b>Tribhuvan University</b></p>
            <div class="abouttu">
                <img class="tuimg" src="./assets/img/tu.jpg" alt="Image of Tribhuvan University main office" />
                <div>
                    <p>
                        Tribhuvan University is a public university located in Kirtipur,
                        Kathmandu, Nepal. Established in 1959, TU is the oldest and the
                        largest university in Nepal. The university offers 1000
                        undergraduate and 500 postgraduate programs across a wide range
                        of disciplines. Additionally, the institution has 62 constituent
                        campuses and over 1080 affiliated colleges across the country.
                    </p>
                    <p>
                        Since its inception, the state-owned university has expanded its
                        programmes. There are five technical institutes and four general
                        faculties.
                    </p>
                    <br />
                    <a class="btnTU" href="https://tu.edu.np">Explore programs</a>
                </div>
            </div>
        </section>
    </main>

    <section class="whyWhat">
        <div>
            <p class="titleSlim">Why study at <b>RR Campus</b>?</p>
            <ol class="whyOL">
                <li>
                    <p><span class="round">1</span></p>
                    <div>
                        <p class="liTitle">Dedicated Faculties</p>
                        <p>At our campus, you'll find dedicated faculties who are
                            committed to providing personalized attention and support to
                            every student. With a focus on mentorship and guidance, our
                            faculty members ensure that each student reaches their fullest
                            potential.
                        </p>
                    </div>
                </li>

                <li>
                    <p><span class="round">2</span></p>
                    <div>
                        <p class="liTitle">Fostering Environment</p>
                        <p>
                            Our campus prides itself on cultivating a nurturing and
                            inclusive environment where students can thrive both
                            academically and personally. From collaborative learning
                            spaces to supportive student services, we strive to create a
                            community where everyone feels valued and empowered to
                            succeed.
                        </p>
                    </div>
                </li>

                <li>
                    <p><span class="round">3</span></p>
                    <div>
                        <p class="liTitle">State-of-the-Art Facilities</p>
                        <p>
                            We are equipped with state-of-the-art facilities that enhance
                            the learning experience and promote innovation. From
                            cutting-edge laboratories to modern libraries, our campus
                            provides the resources and infrastructure necessary for
                            academic excellence.
                        </p>
                    </div>
                </li>

                <li>
                    <p><span class="round">4</span></p>
                    <div>
                        <p class="liTitle">Diverse Learning Opportunities</p>
                        <p>
                            Our campus offers a wide range of academic programs and
                            extracurricular activities designed to cater to diverse
                            interests and talents.
                        </p>
                    </div>
                </li>
            </ol>
        </div>

        <div class="programs">
            <p class="titleSlim" id="progs"><b>Programs</b> Offered</p>
            <h3>Undergraduate Programs</h3>
            <ol class="programOL">
                <li>Bachelor of Computer Application</li>
                <li>Bachelor of Arts in Sociology</li>
                <li>Bachelor of Arts in Anthropology</li>
                <li>Bachelor of Arts in English</li>
                <p>etc...</p>
            </ol>

            <h3>Graduate Programs</h3>
            <ol class="programOL">
                <li>Masters in Economics</li>
                <li>Masters in Nepali</li>
                <li>Masters in English</li>
                <li>Masters in Political Science</li>
                <p>etc...</p>
            </ol>
            <button class="btnSecondary">Explore all programs</button>
        </div>
    </section>

    <section class="notice">
        <p class="titleSlim">Recent <b>Notices</b></p>
        <table>
            <thead>
            <tr>
                <th>Date</th>
                <th>Notice</th>
            </tr>
            </thead>
            <tbody>
            <tr>
                <td>1 Feb</td>
                <td><a href="#">Admission call for 2024</a></td>
            </tr>
            <tr>
                <td>5 Feb</td>
                <td><a href="#">Sports Week program list</a></td>
            </tr>
            <tr>
                <td>7 Feb</td>
                <td><a href="#">Call for research papers</a></td>
            </tr>
            <tr>
                <td>10 Feb</td>
                <td>
                    <a href="#">Course work calender for new year published</a>
                </td>
            </tr>
            <tr>
                <td>12 Feb</td>
                <td><a href="#">Result of 1st semester BCA published</a></td>
            </tr>
            <tr>
                <td colspan="2" style="text-align: center">
                    <a href="#">View more</a>
                </td>
            </tr>
            </tbody>
        </table>
    </section>
</div>

<footer>
    <div class="footer-main">
        <div>
            <img class="logo" src="./assets/img/logo2.png" alt="logo" />
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

</html
