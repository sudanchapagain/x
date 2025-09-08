package main

import (
	"bufio"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"unicode"

	"golang.org/x/text/unicode/norm"
)

func normalizeFilename(filename string) string {
	t := norm.NFD.String(filename)
	t = strings.Map(func(r rune) rune {
		if unicode.Is(unicode.Mn, r) {
			return -1
		}
		return r
	}, t)

	t = strings.ReplaceAll(t, " ", "-")
	t = strings.ReplaceAll(t, "--", "-")
	return t
}

func hashFileContent(filepath string) (string, error) {
	file, err := os.Open(filepath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	hash := sha256.New()
	if _, err := io.Copy(hash, file); err != nil {
		return "", err
	}

	return fmt.Sprintf("%x", hash.Sum(nil)), nil
}

func compareFiles(file1, file2 string) (bool, error) {
	f1, err := os.Open(file1)
	if err != nil {
		return false, err
	}
	defer f1.Close()

	f2, err := os.Open(file2)
	if err != nil {
		return false, err
	}
	defer f2.Close()

	scanner1 := bufio.NewScanner(f1)
	scanner2 := bufio.NewScanner(f2)

	for scanner1.Scan() && scanner2.Scan() {
		line1 := strings.TrimSpace(scanner1.Text())
		line2 := strings.TrimSpace(scanner2.Text())

		if line1 != line2 {
			return false, nil
		}
	}

	if scanner1.Scan() != scanner2.Scan() {
		return false, nil
	}

	return true, nil
}

func main() {
	fmt.Print("Enter the source folder (e.g., '.' or '..'): ")
	var sourceFolder string
	fmt.Scanln(&sourceFolder)

	fmt.Print("Enter the destination folder (e.g., './normalized'): ")
	var destFolder string
	fmt.Scanln(&destFolder)

	absSource, err := filepath.Abs(sourceFolder)
	if err != nil {
		fmt.Println("Error resolving source path:", err)
		return
	}

	absDest, err := filepath.Abs(destFolder)
	if err != nil {
		fmt.Println("Error resolving destination path:", err)
		return
	}

	if _, err := os.Stat(absDest); os.IsNotExist(err) {
		if err := os.MkdirAll(absDest, os.ModePerm); err != nil {
			fmt.Println("Error creating destination folder:", err)
			return
		}
	}

	fileHashes := make(map[string]string)

	err = filepath.Walk(absSource, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		fileHash, err := hashFileContent(path)
		if err != nil {
			fmt.Println("Error hashing file:", err)
			return nil
		}

		normalizedFilename := normalizeFilename(info.Name())
		normalizedPath := filepath.Join(absDest, normalizedFilename)

		if existingFilePath, found := fileHashes[fileHash]; found {
			areEqual, err := compareFiles(path, existingFilePath)
			if err != nil {
				fmt.Println("Error comparing files:", err)
				return nil
			}

			if areEqual {
				fmt.Printf("Duplicate file found: %s (original: %s)\n", path, existingFilePath)
				return nil
			}
		}

		fmt.Printf("Copying file: %s to %s\n", path, normalizedPath)
		if err := copyFile(path, normalizedPath); err != nil {
			fmt.Println("Error copying file:", err)
		}

		fileHashes[fileHash] = normalizedPath

		return nil
	})

	if err != nil {
		fmt.Println("Error walking through source folder:", err)
	}
}

func copyFile(src, dst string) error {
	sourceFile, err := os.Open(src)
	if err != nil {
		return err
	}
	defer sourceFile.Close()

	destFile, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer destFile.Close()

	_, err = io.Copy(destFile, sourceFile)
	if err != nil {
		return err
	}

	return destFile.Sync()
}
