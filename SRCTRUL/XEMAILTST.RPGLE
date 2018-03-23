000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120
000500060317     Ftfntc00f  UF   E             DISK
000600041215     Ftabel00f  IF   E           K DISK
000700001120     FPRTF198   O    F  198        PRINTER
000800001120
000900001120     D*-----------------
001000001120     D* DS DI PROCEDURA
001100001120     D*-----------------
001200001120     D DSEMAIL       E DS
001300041215     D*------------------
001400041215     D* DS DI LETTURA TABELLA "1T"
001500041215     D*------------------
001600041215     D DS1T          E DS
001700001120
001800041215     D*-----------------
001900041215     D* TABELLE CAPICONTO
002000041215     D*-----------------
002100041215     D sk1T            S              2    DIM(300)
002200000411
002300001120     D*-----------------
002400001120     D* VARIABILI DI WRK
002500001120     D*-----------------
002600041215     D riga            S            198    inz(*blanks)
002700041215     D j1T             S              3  0
002800060317     D savEMLINDI      S                   like(§EMLINDI) inz(*blanks)
002900001120
003000001120
003100000414     C*------------------------------------------------------------------------*
003200000414     C* MAIN LINE
003300000414     C*------------------------------------------------------------------------*
003400041215     C*
003500041215     C* CHAIVI DI LETTURA
003600041215     C     KEYTAB        KLIST                                                  *TABEL00F
003700041215     C                   KFLD                    TBLKUT                          -UTENTE
003800041215     C                   KFLD                    TBLCOD                          -TABELLA
003900041215     C*
004000041215     C* CARICO TABELLE
004100041215     C                   EXSR      CARTAB
004200000414     C*
004300041215     C* LEGGO TUTTO IL FILE NOTE E STAMPO EVENTUALI ERRORI
004400041215     C                   READ      tfntc00f
004500041215     C                   DOW       not %eof(tfntc00f)
004600041215     C                   IF        ntcFLT <> 'A'
004700041215     C     ntcTNT        LOOKUP    sk1T                                   55
004800041215     C                   IF        %found
004900041215     C                   EVAL      §EMLINDI = ntcRNT
005000041215     C                   CALL(e)   'XEMAIL'
005100001120     C                   PARM                    DSEMAIL
005200041215     C                   IF        %error         OR
005300041215     C                             §EMLERRO = '1'
005400060317     C*
005500060317     C* Salvo l'indirizzo email originale
005600060317     C                   EVAL      savEMLINDI = §EMLINDI
005700060317     C*
005800060317     C                   IF        LANCIO = *blanks
005900060317     C                   EVAL      riga = ntcAPL+' '+ntcNK1+' '+ntcTNT+' '  +
006000060317     C                                    %trim(savEMLINDI)+' '             +
006100060317     C                                    %trim(§EMLINDO)+' '+%trim(§EMLMSGO)
006200060317     C                   ELSE
006300060317     C*
006400060317     C* Se richiesto nel lancio sistemo alcune tipologie d errore
006500060317     C                   EVAL      §EMLINDI = %trim(§EMLINDI)
006600060317     C*
006700060317     C* ....sistemo: se ultimo carattere è "." => metto *blanks
006800060317     C                   IF        %subst(§EMLINDI:%len(%trim(§EMLINDI)):1)='.'
006900060317     C                   EVAL      %subst(§EMLINDI:%len(%trim(§EMLINDI)):1)=' '
007000060317     C                   ENDIF
007100060317     C*
007200060317     C* ....sistemo: se ultimi 2 caratteri sono ".i" => metto '.it'
007300060317     C                   IF        %subst(§EMLINDI:
007400060317     C                             %len(%trim(§EMLINDI))-1:2)='.i'
007500060317     C                   EVAL      %subst(§EMLINDI:
007600060317     C                             %len(%trim(§EMLINDI))-1:2)=*blanks
007700060317     C                   EVAL      §EMLINDI=%trim(§EMLINDI)+'.it'
007800060317     C                   ENDIF
007900060317     C*
008000060317     C* ....sistemo: se ultimi 2 caratteri sono ".t" => metto '.it'
008100060317     C                   IF        %subst(§EMLINDI:
008200060317     C                             %len(%trim(§EMLINDI))-1:2)='.t'
008300060317     C                   EVAL      %subst(§EMLINDI:
008400060317     C                             %len(%trim(§EMLINDI))-1:2)=*blanks
008500060317     C                   EVAL      §EMLINDI=%trim(§EMLINDI)+'.it'
008600060317     C                   ENDIF
008700060317     C*
008800060317     C* ....sistemo: se o subito prima o subito dopo la "@" c'è il "." => tolgo il "."
008900060317     C                   IF        %subst(§EMLMSGO:1:1) = '9'
009000060317     C                   Z-ADD     *zeros        PosAt             3 0
009100060317     C     '§':'@'       XLATE     §EMLINDI      §EMLINDI
009200060317     C                   EVAL      PosAt = %scan('@':§EMLINDI)
009300060317     C                   IF        (%scan('.':§EMLINDI) = PosAt + 1)
009400060317     C                   EVAL      §EMLINDI=%subst(§EMLINDI:1:PosAt)+
009500060317     C                                      %subst(§EMLINDI:PosAt+2)
009600060317     C                   ENDIF
009700060317     C                   IF        (%scan('.':§EMLINDI) = PosAt - 1)
009800060317     C                   EVAL      §EMLINDI=%subst(§EMLINDI:1:PosAt-2)+
009900060317     C                                      %subst(§EMLINDI:PosAt)
010000060317     C                   ENDIF
010100060317     C                   ENDIF
010200060317     C*
010300060317     C                   IF        LANCIO = 'U'
010400060317     C                   IF        %trim(ntcRNT) <> §EMLINDI
010500060317     C                   EVAL      ntcRNT = §EMLINDI
010600060317     C                   UPDATE    TFNTC
010700060317     C                   ENDIF
010800060317     C                   ENDIF
010900060317     C*
011000060317     C                   EVAL      riga = ntcAPL+' '+ntcNK1+' '+ntcTNT+' '  +
011100060317     C                                    %trim(savEMLINDI)+' '             +
011200060317     C                                    %trim(§EMLINDI)+' '+%trim(§EMLMSGO)
011300060317     C                   ENDIF
011400060317     C*
011500060317     C* Listo l'errore intercettato
011600060317     C                   EXCEPT    dettaglio
011700060317     C*
011800001120     C                   ENDIF
011900041215     C                   ENDIF
012000001124     C                   ENDIF
012100041215     C                   READ      tfntc00f
012200001120     C                   ENDDO
012300001120     C*
012400001120     C                   SETON                                        LR
012500041215     C*------------------------------------------------------------------------*
012600041215     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
012700041215     C*------------------------------------------------------------------------*
012800041215     C     CARTAB        BEGSR
012900041215     C*---
013000041215     C* CARICAMENTO TIPI NOTE
013100041215     C*---
013200041215     C                   Z-ADD     *ZEROS        j1T
013300041215     C                   Z-ADD     1             TBLKUT
013400041215     C                   MOVEL     '1T'          TBLCOD
013500041215     C     KEYTAB        SETLL     TABEL00F
013600041215     C     KEYTAB        READE     TABEL00F                               99
013700041215     C     *IN99         DOWEQ     *OFF
013800041215     C     TBLFLG        IFEQ      *BLANKS
013900041215     C                   MOVEL     TBLUNI        DS1T
014000041215     C                   IF        §1TEML = 'S'
014100041215     C                   ADD       1             j1T
014200041215     C                   MOVEL     TBLKEY        sk1T(j1T)
014300041215     C                   ENDIF
014400041215     C                   ENDIF
014500041215     C     KEYTAB        READE     TABEL00F                               99
014600041215     C                   ENDDO
014700041215     C*
014800041215     C                   ENDSR
014900001120
015000060317     C*------------------------------------------------------------------------*
015100060317     C* *INZSR - ROUTINE DI INGRESSO
015200060317     C*------------------------------------------------------------------------*
015300060317     C     *inzsr        BEGSR
015400060317     C*
015500060317     C* RICEVIMENTO PARAMETRI
015600060317     C     *ENTRY        PLIST
015700060317     C                   PARM                    LANCIO            1
015800060317     C*
015900060317     C* ' ' => solo stampa
016000060317     C* 'U' => aggiorna
016100060317     C* 'T' => test pre-aggiorna
016200060317     C*
016300060317     C                   ENDSR
016400001120
016500001120     OPRTF198   E            dettaglio   2
016600001120     O                       riga
016700001120
