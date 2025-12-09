const jspdf = document.createElement("script");

jspdf.onload = () => {
	const pdf = new jsPDF();
	const elements = document.getElementsByTagName("img");
	for (const i in elements) {
		const img = elements[i];
		console.log("add img ", img);

		if (!/^blob:/.test(img.src)) {
			console.log("invalid src");
			continue;
		}

		const can = document.createElement("canvas");
		const con = can.getContext("2d");
		can.width = img.width;
		can.height = img.height;
		con.drawImage(img, 0, 0, img.width, img.height);
		const imgData = can.toDataURL("image/jpeg", 1.0);
		pdf.addImage(imgData, "JPEG", 0, 0);
		pdf.addPage();
	}
	pdf.save("download.pdf");
};

jspdf.src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/1.5.3/jspdf.debug.js";
document.body.appendChild(jspdf);
