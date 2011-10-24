

(define url (new <java.net.URL> "http://www.notam02.no/~kjetism/upload.php"))
(begin url)

(begin
  (define connection (-> url openConnection))
  (-> connection setRequestMethod "POST")
  (-> connection addRequestProperty "filename" "/home/kjetil/testing.wav")
  (-> connection setDoOutput #t)
  (-> connection setDoInput #t)
  (-> connection setRequestProperty "Connection" "Keep-Alive")
  (-> connection setRequestProperty "Content-Type" "multipart/form-data;boundary=*****")
  (-> connection connect))



(define writer (new <java.io.DataOutputStream> (-> connection getOutputStream)))
(-> writer writeBytes "Content-Disposition: form-data; name=\"upload\";")
(-> writer writeBytes " filename=/home/kjetil/testing.wav\n")
(-> writer writeBytes "\n")

(define reader (new <java.io.BufferedReader> (new <java.io.InputStreamReader> (-> connection getInputStream))))

(-> reader readLine)


(define Upload-static (new-static <Upload>))

(-> Upload-static upload "/home/kjetil/testing.wav" "http://www.notam02.no/~kjetism/upload.php")

