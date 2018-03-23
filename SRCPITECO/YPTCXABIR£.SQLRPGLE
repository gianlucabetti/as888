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
004700131015
004800131015
004900131015     C**   *entry        plist
005000131015     C**                 parm                    iidd             15 5
005100130806
005200130726      /free
005300131114               my_file_name = '/bartolini/piteco/anag/'
005400131114                   + 'XABI.txt';
005500131015               my_file_nl = %len(%trim(my_file_name));
005600131015               my_file_fo = SQFAPP;
005700131015
005800131015            // sqfrd-sqfcrt-sqfovr-sqfapp
005900131015
006000131015               callp unlink(%trim(my_file_name));
006100131015
006200131015            // Create an empty file:
006300131015               callp close( open( %trim(my_file_name)
006400131015               : O_WRONLY + O_TRUNC + O_CREAT + O_CCSID
006500131023            // : S_IWUSR + S_IRUSR + S_IRGRP + S_IROTH
006600131015               : RW*OWNER + RW*GROUP + R
006700131015               : cp_windows )
006800131015               );
006900131015
007000131015               exec sql
007100131023         declare S1 cursor for
007200131023         SELECT
007300131023          // chiave
007400131023         char('201', 4)                // societa
007500131023         concat 'XABI'
007600131023         concat char(digits(abiabi) concat // cod. anagrafica
007700131023                     digits(abicab) , 20)
007800131023         concat repeat(' ' , 20)       // filler
007900131023         concat repeat(' ' , 20)       // filler
008000131023          // dati
008100131023         concat 'A'                    // 'A' = automatico
008200131023         concat abiist                 // descrizione banca (40+40)
008300131023         concat repeat(' ' , 8)        // data inizio validità
008400131030         concat case                   // data fine validità
008500131030         when abiann='*' and abidtr > 391231
008600131030         then '19' concat substr(digits(abidtr), 1 , 2)
008700131030         concat substr(digits(abidtr), 3 , 2)
008800131030         concat substr(digits(abidtr), 5 , 2)
008900131030         when abiann='*' and abidtr <= 391231
009000131030         then '20' concat substr(digits(abidtr), 1 , 2)
009100131030         concat substr(digits(abidtr), 3 , 2)
009200131030         concat substr(digits(abidtr), 5 , 2)
009300131030         else repeat(' ' , 8)   end
009400131023         concat repeat(' ' , 20)       // cod.raggr. facoltativo
009500131023         concat repeat(' ' , 16)       // cod. fiscale
009600131023         concat repeat(' ' , 1)        // flag libero
009700131023         concat repeat(' ' , 1)        // flag libero
009800131023         concat repeat(' ' , 1)        // flag libero
009900131030          // indirizzo anagrafica
010000131030         concat case abipro when 'SM'  // paese
010100131030         then 'SM ' else 'IT ' end
010200131030         concat repeat(' ' , 40)       // indirizzo
010300131030         concat repeat(' ' , 30)       // località
010400131030         concat repeat(' ' , 5)        // cap
010500131030         concat repeat(' ' , 2)        // provincia
010600131030         concat repeat(' ' , 5)        // abi
010700131030         concat repeat(' ' , 5)        // cab
010800131023         concat repeat(' ' , 12)       // filler
010900131023         concat repeat(' ' , 20)       // filler
011000131023          // dati appoggio
011100131030         concat substr(abiage , 1 , 50)  // descrizione
011200131030         concat substr(abiage , 1 , 30)  // sportello
011300131030         concat substr(abind  , 1 , 40)  // indirizzo
011400131030         concat substr(abiloc , 1 , 30)  // località
011500131030         concat digits(abicap)           // cap
011600131030         concat abipro                   // provincia
011700131023         concat digits(abiabi)           // abi
011800131023         concat digits(abicab)           // cab
011900131030         concat digits(abiabi)         // cod. intern. abi+cab+2spazi
012000131030         concat digits(abicab)
012100131030         concat repeat(' ' , 2)
012200131030         concat repeat(' ',  50)       // nr. conto
012300131023         concat repeat(' ' , 20)       // filler
012400131023          // end of record
012500131023         concat :CRLF
012600131030               as cloclo from cnabi00f
012700131015                        where abiist <> '';
012800131015
012900131015               exec sql  open S1    ;
013000131015
013100131015               Dow Sqlcode = 0;
013200131015               exec sql
013300131015               fetch  S1 into :my_file;
013400131015               enddo;
013500131015
013600131015               Exec Sql Close Mycursor;
013700131015
013800130806               *inlr = *on;
013900130806               return;
014000130806      /end-free
