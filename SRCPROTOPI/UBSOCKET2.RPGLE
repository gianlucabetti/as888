000100081118     D*------------------
000200090710     D* PASSAGGIO PARAMETRI A PROCEDURE UBSOCKET
000300081118     D*------------------
000400100922     D pInHost         S            256A
000500090710     D pInPort         S              5U 0
000600151126     D pInSktDescr     S             10I 0
000700160331     D pInSktMsg       S           4096A
000800151009     D pInLocalAdr     S            256A
000900151009     D pInLocalPort    S              5U 0
001000151216     D pInMsgCvtTbl    S             10A
001100151009     D pInBackLog      S              5U 0
001200151126     D pInBufferLen    S             10I 0
001300151126     D pInSktType      S             10A
001400151126     D pInSktLevel     S             10I 0
001500151126     D pInOptName      S             10I 0
001600151126     D pInOptValue     S             10I 0
001700151127     D pInByteNo       S              5I 0
001800151127     D pInBitMask      S              1A
001900151127     D pInFD           S             10I 0
002000151127     D pInFDset        S             28A
002100151127     D pInCommand      S             10I 0
002200151127     D pInCmdArg       S             10I 0
002300151202     D pInMaxDescr     S             10I 0
002400151202     D pInReadSet      S             28A
002500151202     D pInWriteSet     S             28A
002600151202     D pInExcpSet      S             28A
002700151202     D pInWaitTime     S             10I 0
002800151216     D pInSktAddrPtr   S               *
002900151202     D pOutSktDescr    S             10I 0
003000151012     D pOutSktAddr     S             15A
003100151212     D pOutRespPtr     S               *
003200151216     D pOutSktAddrPtr  S               *
003300160331     D pOutRespTxt     S           4096A
003400151212     D pOutRespLen     S             10I 0
003500151212     D pOutRespFull    S             10I 0
003600151127     D*------------------
003700151127     D* VARIABILI DI COMODO
003800151127     D*------------------
003900151127     D fdset           S             28A
004000151126     D*------------------
004100151126     D* STRUTTURE DATI DI BASE RICORRENTI
004200151126     D*------------------
004300151012     D p_sockaddr      S               *
004400151012     D sockaddr        DS                  based(p_sockaddr)
004500151012     D   sa_family                    5I 0
004600151012     D   sa_data                     14A
004700151012     D sockaddr_in     DS                  based(p_sockaddr)
004800151012     D   sin_family                   5I 0
004900151012     D   sin_port                     5U 0
005000151126     D   sin_addr                    10U 0
005100151012     D   sin_zero                     8A
005200151127     D p_connfrom      S               *
005300151127     D p_connto        S               *
005400151127     D p_bindto        S               *
005500151127     D addrlen         S             10I 0
005600151126     D*------------------
005700151127     D p_timeval       S               *
005800151127     D timeval         DS                  based(p_timeval)
005900151127     D   tv_sec                      10I 0
006000151127     D   tv_usec                     10I 0
006100151127     D p_tv            S               *
006200151127     D*------------------
006300151127     D p_linger        S               *
006400151127     D linger          DS                  based(p_linger)
006500151127     D   l_onoff                     10I 0
006600151127     D   l_linger                    10I 0
006700151127     D p_linge         S               *
006800151127     D*------------------
006900151127     D* COSTANTI DI "PROCEDURA"
007000151127     D*------------------
007100151127     D
007200151127     D* Address Family
007300151127     D AF_INET         C                   CONST(2)                             * TCP
007400151127     D AF_IPX          C                   CONST(6)                             * IPX/SPX
007500151127     D AF_NETBIOS      C                   CONST(17)                            * NetBIOS
007600151127     D AF_INET6        C                   CONST(23)                            * TCP
007700151127     D AF_IRDA         C                   CONST(26)                            * Infrared
007800151127     D AF_BTH          C                   CONST(32)                            * BlueTooth
007900151127     D
008000151127     D* Protocol
008100151127     D IPPROTO_IP      C                   CONST(0)                             * Generic IP
008200151127     D IPPROTO_ICMP    C                   CONST(1)                             * ICMP
008300151127     D BTHPROTO_RFCOM  C                   CONST(3)                             * BlueTooth
008400151127     D IPPROTO_TCP     C                   CONST(6)                             * TCP
008500151127     D IPPROTO_UDP     C                   CONST(17)                            * UDP
008600151127     D IPPROTO_ICMPV6  C                   CONST(58)                            * ICMP V6
008700151127     D IPPROTO_RM      C                   CONST(113)                           * PGM
008800151127     D IPPROTO_RAW     C                   CONST(255)                           * RAW
008900151127     D SOL_SOCKET      C                   CONST(-1)                            * SOCKET LAYER
009000151127     D
009100151127     D* IP Level
009200151127     D IP_OPTIONS      C                   CONST(5)                             * IP Options
009300151127     D IP_TOS          C                   CONST(10)                            * Type of service
009400151127     D IP_TTL          C                   CONST(15)                            * Time To Live
009500151127     D IP_RECVLCLIFADDR...                                                      * Recv Lcl IFC addr
009600151127     D                 C                   CONST(99)
009700151127     D
009800151127     D* Internet Address special value
009900151127     D INADDR_ANY      C                   CONST(0)
010000151127     D INADDR_BROADCAST...
010100151127     D                 C                   CONST(4294967295)
010200151127     D INADDR_LOOPBACK...
010300151127     D                 C                   CONST(2130706433)
010400151127     D
010500151127     D* Internet Address special value
010600151127     D TCP_MAXSEG      C                   CONST(5)                             * Max Segment Size
010700151127     D TCP_NODELAY     C                   CONST(10)                            * NoDelay Small-Pack
010800151127     D
010900151127     D* Socket Level
011000151127     D SO_BROADCAST    C                   CONST(5)                             * allw broadcast msg
011100151127     D SO_DEBUG        C                   CONST(10)                            * record dbg inform.
011200151127     D SO_DONTROUTE    C                   CONST(15)                            * bypass routing
011300151127     D SO_ERROR        C                   CONST(20)                            * error status
011400151127     D SO_KEEPALIVE    C                   CONST(25)                            * keep conns alive
011500151127     D SO_LINGER       C                   CONST(30)                            * linger upon close
011600151127     D SO_OOBINLINE    C                   CONST(35)                            * OutOfBandDataInLin
011700151127     D SO_RCVBUF       C                   CONST(40)                            * rcv buffer size
011800151127     D SO_RCVLOWAT     C                   CONST(45)                            * rcv low water mark
011900151127     D SO_RCVTIMEO     C                   CONST(50)                            * rcv timeout value
012000151127     D SO_REUSEADDR    C                   CONST(55)                            * re-use local addrs
012100151127     D SO_SNDBUF       C                   CONST(60)                            * snd buffer size
012200151127     D SO_SNDLOWAT     C                   CONST(65)                            * snd low water mark
012300151127     D SO_SNDTIMEO     C                   CONST(70)                            * send timeout value
012400151127     D SO_TYPE         C                   CONST(75)                            * socket type
012500151127     D SO_USELOOPBACK  C                   CONST(80)                            * send loopback
012600151127     D
012700151127     D* Socket Type
012800151127     D SOCK_STREAM     C                   CONST(1)                             * TCP
012900151127     D SOCK_DGRAM      C                   CONST(2)                             * UDP
013000151127     D SOCK_RAW        C                   CONST(3)                             * IP Datagram
013100151127     D SOCK_RDM        C                   CONST(4)                             * PGM
013200151127     D SOCK_SEQPACKET  C                   CONST(5)                             * ???
013300151127     D
013400151127     D* fcntl() commands
013500151127     D F_GETFL         C                   CONST(6)
013600151127     D F_SETFL         C                   CONST(7)
013700151127     D
013800151127     D* fcntl() flags
013900151127     D O_NONBLOCK      C                   CONST(128)
014000151127     D O_NDELAY        C                   CONST(128)
014100151127     D FNDELAY         C                   CONST(128)
014200151127     D
014300151127     D*------------------
