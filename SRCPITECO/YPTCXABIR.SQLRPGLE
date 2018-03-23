000100130726     H DECEDIT('0,') DATEDIT(*DMY.)   datfmt(*iso)
000200130726     H CVTOPT(*VARCHAR)  DFTACTGRP(*NO) ACTGRP('QILE')
000300130726
000400131015     D MY_FILE         S                   SQLTYPE(CLOB_FILE)
000500131015     D CRLF            S              2    inz(X'0D25')
000600131015
000700131015     D*------------
000800131015     D open            PR            10I 0 extproc('open')
000900131015     D   path                          *   value options(*string)
001000131015     D   oflag                       10I 0 value
001100131015     D   mode                        10U 0 value options(*nopass)
001200131015     D   codepage                    10U 0 value options(*nopass)
001300131015
001400131015     D write           PR            10I 0 extproc('write')
001500131015     D   fildes                      10I 0 value
001600131015     D   buf                           *   value
001700131015     D   nbyte                       10U 0 value
001800131015
001900131015     D close           PR            10I 0 extproc('close')
002000131015     D   fildes                      10I 0 value
002100131015
002200131015     D unlink          PR            10I 0 ExtProc('unlink')
002300131015     D   path                          *   Value options(*string)
002400131015
002500131015     D filename        S           1000A   Varying
002600131015     D fd              S             10I 0
002700131015C2163X*** data            S            200A
002800131015C2163D data            S           2000A
002900131015     D msg             S             52A
003000131015
003100131015     D O_WRONLY        C                   2
003200131015     D O_CREAT         C                   8
003300131015     D O_CCSID         C                   32
003400131015     D O_TRUNC         C                   64
003500131015     D O_TEXTDATA      C                   16777216
003600131015     D O_CODEPAGE      C                   8388608
003700131015
003800131015     D RW              C                   6
003900131015     D R               C                   4
004000131015     D OWNER           C                   64
004100131015     D GROUP           C                   8
004200131015     D*
004300131015     D* Il Codepage 1252 è la variante MS-WIndows del Latin-1 ASCII
004400131015     D CP_WINDOWS      C                   1252
004500131015     D*----------------------------------------------------*
004600131015
004700131128     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
004800131015
004900131015     C**   *entry        plist
005000131015     C**                 parm                    iidd             15 5
005100130806
005200130726      /free
005300131125         // creo in tmp invece che in Anag perchè
005400131125         // Mimix mi alloca il file durante la scrittura.
005500131125         //      my_file_name = '/bartolini/piteco/anag/'
005600131128               my_file_name = '/tmp/'
005700131128                   + 'XABI.TXT';
005800131015               my_file_nl = %len(%trim(my_file_name));
005900131015               my_file_fo = SQFAPP;
006000131015
006100131015            // sqfrd-sqfcrt-sqfovr-sqfapp
006200131015
006300131015               callp unlink(%trim(my_file_name));
006400131015
006500131015            // Create an empty file:
006600131015               callp close( open( %trim(my_file_name)
006700131015               : O_WRONLY + O_TRUNC + O_CREAT + O_CCSID
006800131023            // : S_IWUSR + S_IRUSR + S_IRGRP + S_IROTH
006900131015               : RW*OWNER + RW*GROUP + R
007000131015               : cp_windows )
007100131015               );
007200131015
007300131015               exec sql
007400131023         declare S1 cursor for
007500131023         SELECT
007600131023          // chiave
007700131023         char('201', 4)                // societa
007800131023         concat 'XABI'
007900131023         concat char(digits(abiabi) concat // cod. anagrafica
008000131023                     digits(abicab) , 20)
008100131023         concat repeat(' ' , 20)       // filler
008200131023         concat repeat(' ' , 20)       // filler
008300131023          // dati
008400131023         concat 'A'                    // 'A' = automatico
008500131023         concat abiist                 // descrizione banca (40+40)
008600131023         concat repeat(' ' , 8)        // data inizio validità
008700131030         concat case                   // data fine validità
008800131030         when abiann='*' and abidtr > 391231
008900131030         then '19' concat substr(digits(abidtr), 1 , 2)
009000131030         concat substr(digits(abidtr), 3 , 2)
009100131030         concat substr(digits(abidtr), 5 , 2)
009200131030         when abiann='*' and abidtr <= 391231
009300131030         then '20' concat substr(digits(abidtr), 1 , 2)
009400131030         concat substr(digits(abidtr), 3 , 2)
009500131030         concat substr(digits(abidtr), 5 , 2)
009600131030         else repeat(' ' , 8)   end
009700131023         concat repeat(' ' , 20)       // cod.raggr. facoltativo
009800131023         concat repeat(' ' , 16)       // cod. fiscale
009900131023         concat repeat(' ' , 1)        // flag libero
010000131023         concat repeat(' ' , 1)        // flag libero
010100131023         concat repeat(' ' , 1)        // flag libero
010200131030          // indirizzo anagrafica
010300131030         concat case abipro when 'SM'  // paese
010400131030         then 'SM ' else 'IT ' end
010500131030         concat repeat(' ' , 40)       // indirizzo
010600131030         concat repeat(' ' , 30)       // località
010700131030         concat repeat(' ' , 5)        // cap
010800131030         concat repeat(' ' , 2)        // provincia
010900131030         concat repeat(' ' , 5)        // abi
011000131030         concat repeat(' ' , 5)        // cab
011100131023         concat repeat(' ' , 12)       // filler
011200131023         concat repeat(' ' , 20)       // filler
011300131023          // dati appoggio
011400131030         concat substr(abiage , 1 , 50)  // descrizione
011500131030         concat substr(abiage , 1 , 30)  // sportello
011600131030         concat substr(abind  , 1 , 40)  // indirizzo
011700131030         concat substr(abiloc , 1 , 30)  // località
011800131030         concat digits(abicap)           // cap
011900131030         concat abipro                   // provincia
012000131023         concat digits(abiabi)           // abi
012100131023         concat digits(abicab)           // cab
012200131030         concat digits(abiabi)         // cod. intern. abi+cab+2spazi
012300131030         concat digits(abicab)
012400131030         concat repeat(' ' , 2)
012500131030         concat repeat(' ',  50)       // nr. conto
012600131023         concat repeat(' ' , 20)       // filler
012700131023          // end of record
012800131023         concat :CRLF
012900131030               as cloclo from cnabi00f
013000131015                        where abiist <> '';
013100131015
013200131015               exec sql  open S1    ;
013300131015
013400131015               Dow Sqlcode = 0;
013500131015               exec sql
013600131015               fetch  S1 into :my_file;
013700131015               enddo;
013800131015
013900131015               Exec Sql Close Mycursor;
014000131015
014100131128               if executeCommand(
014200131129                'DEL OBJlnk(''/BARTOLINI/PITECO/ANAG/XABI.TXT'')')
014300131128                <> 0;
014400131128            //   rpyEsito = -1;
014500131128            //   rpyOpCode = exceptionMessageID;
014600131128               endif;
014700131128
014800131129               if executeCommand(
014900131129                'MOV toOBJ(''/BARTOLINI/PITECO/ANAG/XABI.TXT'') +
015000131129                OBJ(''/tmp/xabi.txt'')')
015100131129                <> 0;
015200131129            //   rpyEsito = -1;
015300131129            //   rpyOpCode = exceptionMessageID;
015400131129               endif;
015500131129
015600130806               *inlr = *on;
015700130806               return;
015800130806      /end-free
