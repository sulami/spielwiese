package main

import (
	"flag"
	"net"
)

func worker(done chan bool, addr string, rnum int) {
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		done <- false
		return
	}

	for i := 0; i < rnum; i++ {
		req := []byte("set blub 14 5 noreply\r\nvalue\r\n")
		conn.Write(req)
	}

	conn.Write([]byte("quit\r\n"))
	conn.Close()

	done <- true
	return
}


func main() {
	host := flag.String("host", "localhost:11211", "host to connect to")
	wnum := flag.Int("workers", 10, "number of parallel workers to use")
	rnum := flag.Int("requests", 1000, "number of requests/worker to send")
	flag.Parse()

	done := make(chan bool)

	for i := 0; i < *wnum; i++ {
		go worker(done, *host, *rnum)
	}

	for i := 0; i < *wnum; i++ {
		rv := <-done
		if rv {
			println("Worker", i, "done")
		} else {
			println("Worker", i, "failed")
		}
	}

	return
}

