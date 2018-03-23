000100970526     H*------------------------------------------------------------------------*
000200170914     H* Legami Depot DPD/LEG - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170919     FTISIe5d1  CF   E             WORKSTN INFDS(DEVDS1)
000900060906     F                                     SFILE(SIE5DMS:MSGULT)
001000170914     FDPLEG11L  UF   E           K DISK
001100171122     FDPimd12L  iF   E           K DISK    INFDS(IMDDS)
001200171122     FDPimd10F  uF   E             DISK    rename(DPIMD100:dpimdFIS)
001300170915     Fdpcdp11i  IF   E           K DISK
001400171122     Fdpcbu11i  IF   E           K DISK
001500060906     Ftabel00f  IF   E           K DISK
001600150925     Ftntbe01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000170918     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
002100970526     D*------------
002200970526     D* DECODIFICHE
002300970526     D*------------
002400970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* MESSAGGI
002700970526     D*------------
002800970526     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI TASTI FUNZIONALI
003100970526     D*------------
003200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* FUNZIONI DA CARICARE A VIDEO
003500970526     D*------------
003600970526     D FUV             S             19    DIM(24)
003700970526     D*------------------------------------------------------------------------*
003800970526     D* INPUT
003900970526     D*------------------------------------------------------------------------*
004000970526     D*------------
004100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004200970526     D*------------
004300970526     D FUDDS           DS
004400970526     D  FUDDE1                 1      1
004500970526     D  FUDFUN                 2      3
004600970526     D  FUDDE2                 4     19
004700970526     D  FUDFI0                20     34
004800970526     D  FUDATT                35     35
004900970526     D  FUDFI1                36     80
005000970526     D  FUDDES                 1     19
005100970526     D  FUDRIG                 1     80
005200970526     D*------------
005300970526     D* DS GESTIONE TASTI FUNZIONALI
005400970526     D*------------
005500970526     D DSKEY         E DS
005600970526     D DEVDS1          DS
005700970526     D  KEY                  369    369
005800160524     D*-------------
005900160524     d IMDds           ds
006000160524     d  imd_nrr              397    400b 0
006100160524      *
006200160524     d  sav_imdnrr     s                   like(IMD_nrr)
006300060907     D*------------
006400060907     D* DS REPERIMENTO DATI UTENTE
006500060907     D*------------
006600060907     D TIBS34DS      E DS                                                       *Profili utente
006700060907     D DDATIUTE      E DS                                                       *Dati utente
006800060907     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900170914     D DLEGFLO       E DS
007000060907     D*------------------
007100060907     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200060907     D*------------------
007300060907     D WLBDA8          DS                  INZ
007400060907     D  G08DAT                 1      8  0
007500060907     D  G08INV                 9     16  0
007600060907     D  G08ERR                17     17
007700060907     D  G08TGI                18     22  0
007800970526     D*------------
007900970526     D* DS "XMSG" - GESTIONE MESSAGGI
008000970526     D*------------
008100970526     D DSMSG         E DS
008200970526     D*------------------
008300970526     D* DS NAZIONI
008400970526     D*------------------
008500970526     D DS15          E DS
008600970526     D*------------------
008700970526     D* DS DI PROCEDURA
008800970526     D*------------
008900170919     D tisie5ds      E DS
009000060906     D*------------
009100060907      * - Ds per Anagrafiche Clienti
009200060907     d TIBS69DS      E DS
009300060907     d DS_cnaco      E DS                  extname(CNACO00F)
009400060907     d DS_cnind      E DS                  extname(CNIND00F)
009500060907     d DS_cnclp      E DS                  extname(CNCLP00F)
009600060907     d DS_fncls      E DS                  extname(FNCLS00F)
009700970526     D*------------
009800970526     D* ARCHITETTURA
009900970526     D*------------
010000970526     D KPJBA         E DS
010100060906     d
010200060906     d dataiso         s               d   datfmt(*iso)
010300060906     d dataeur         s               d   datfmt(*eur)
010400970526     C*------------------------------------------------------------------------*
010500970526     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* NON CI SONO ERRORI IN ENTRATA
010900970526IF  1C     ERRL00        IFEQ      '0'
011000970526     C*
011100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011500970526     C*
011600970526     C* EMETTE FORMATO VIDEO
011700970526     C                   EXSR      BUTFMR
011800970526     C*
011900970526     C* GESTIONE TASTI FUNZIONALI
012000970526     C                   EXSR      FUNCON
012100970526E   2C                   ENDDO
012200970526     C*
012300970526     C* CI SONO ERRORI IN ENTRATA
012400970527X   1C                   ELSE
012500060906     C                   MOVEL     '1'           De5ERR
012600060906     C                   MOVEL     DSMSMS        De5MSG
012700970526E   1C                   ENDIF
012800970526     C* FINE PGM
012900060907     C                   eval      I69TLA  = 'C'
013000060907     C                   call      'TIBS69R'
013100060907     C                   parm                    TIBS69DS
013200060907     c*
013300970526     C                   SETON                                        LR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013600970526     C*------------------------------------------------------------------------*
013700970526     C     BUTFMR        BEGSR
013800970526     C*
013900970526     C* EMETTE LA TESTATA
014000060906     C                   WRITE     SIe5D01
014100970526     C*
014200970526     C* SPEDISCE EVENTUALE MESSAGGIO
014300970526     C                   EXSR      SNDMSG
014400970526     C*
014500970526     C* EMETTE LE FUNZIONI
014600060906     C                   WRITE     SIe5D02
014700970526     C*
014800000121     C* EMETTE IL VIDEO ED ESEGUE LA DECODIFICA
014900000120     C                   EXSR      DECODI
015000060906     C                   EXFMT     SIe5D03
015100970526     C*
015200970526     C* REIMPOSTA FLAG DI ERRORE
015300970526     C                   MOVEL     '0'           ERRL00
015400150925     c                   clear                   dsmsms
015500000121     C* REIMPOSTA INDICATORI DI ERRORE
015600150925     C                   SETOFF                                       606165
015700970526     C*
015800970526     C                   ENDSR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016100970526     C*------------------------------------------------------------------------*
016200970526     C     SNDMSG        BEGSR
016300970526     C*
016400970526     C* PULISCE IL SFILE MESSAGGI
016500970526     C                   MOVEL     '1'           *IN40
016600060906     C                   WRITE     SIe5DMC
016700970526     C                   MOVEL     '0'           *IN40
016800970526     C*
016900970526     C* VISUALIZZA IL MESSAGGIO
017000970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017100970526     C                   CALL      'XMSG'        XMSGPA
017200970526IF  1C     DSMSER        IFEQ      *ZEROS
017300970526     C                   MOVEL     DSMSKE        MSGKEY
017400970526E   1C                   ENDIF
017500970526     C                   Z-ADD     1             MSGULT
017600060906     C                   WRITE     SIe5DMS
017700060906     C                   WRITE     SIe5DMC
017800970526     C*
017900970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018000970526     C                   MOVEL     *BLANKS       DSMSMS
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* ENTGES - GESTIONE ENTER -> INVIO
018500970526     C*------------------------------------------------------------------------*
018600970526     C     ENTGES        BEGSR
018700970526     C*
018800970526     C* CONTROLLA I CAMPI IMMESSI
018900970526     C                   EXSR      DECODI
019000970526     C*
019100970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019200980128IF  1C     DEPOP1        IFEQ      'O05'
019300980128     C                   MOVEL     '1'           FINL00
019400980128E   1C                   ENDIF
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F03GES - GESTIONE F03 -> FINE
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F03GES        BEGSR
020100970526     C*
020200970526     C* IMPOSTA "PREMUTO F03"
020300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020400060906     C                   MOVEL     '1'           De5F03                         *DI PROCEDURA
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
020900970526     C*------------------------------------------------------------------------*
021000970526     C     F06GES        BEGSR
021100970526     C*
021200970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021300970526IF  1C     DEPOP1        IFEQ      'F06'
021400970526     C                   EXSR      INSERI
021500970526E   1C                   ENDIF
021600970526IF  1C     DEPOP1        IFEQ      'O02'
021700970526     C                   EXSR      MODIFI
021800970526E   1C                   ENDIF
021900970526IF  1C     DEPOP1        IFEQ      'O04'
022000970526     C                   EXSR      ANNULL
022100970526E   1C                   ENDIF
022200970526IF  1C     DEPOP1        IFEQ      'O07'
022300970526     C                   EXSR      RIPRIS
022400970526E   1C                   ENDIF
022500970526     C*
022600970526     C                   ENDSR
022700060907     C*------------------------------------------------------------------------*
022800060907     C* F07GES - GESTIONE F07 -> reicerca codice cliente
022900060907     C*------------------------------------------------------------------------*
023000060907     C     F07GEs        BEGSR
023100060907     C*
023200060907     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
023300060907     C                   Z-ADD     3             PAXkut                         tipo estrazione
023400060907     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
023500060907     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
023600060907     c                   movel(P)  Vidrag        PAXrag
023700060907     C                   z-add     1             PAXnum
023800060907      *
023900060907     C                   CALL      'XALFA3BR'
024000060907     C                   PARM                    PAXDUT           30
024100060907     C                   PARM                    PAXkut            1 0
024200060907     C                   PARM                    PAXrag           48
024300060907     C                   PARM                    PAXkcc            4 0
024400060907     C                   PARM                    PAXsta            1 0
024500060907     C                   PARM      *blanks       PAXflr           90
024600060907     C                   PARM      *blanks       PAXdit            3
024700060907     C                   PARM                    PAXnum            2 0
024800060907     C                   PARM                    PAXkcm           80
024900060907     C                   PARM                    PAXksm          140
025000060907     C                   PARM                    PAXkdm           60
025100060907      *
025200060907      * NON TROVATO O PREMUTO CMD12
025300060907    2C                   If        paxsta > -1
025400060907     C                   movel     PAXksm        w007a             7
025500060907     c                   move      w007a         vidksc
025600060907     C                   movel     PAXrag        Vidrag
025700060907     c                   endif
025800060907     c
025900060907     C*
026000060907     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200970526     C* F12GES - GESTIONE F12 -> RITORNO
026300970526     C*------------------------------------------------------------------------*
026400970526     C     F12GES        BEGSR
026500970526     C*
026600970526     C* IMPOSTA "PREMUTO F12"
026700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026800060906     C                   MOVEL     '1'           de5F12                         *DI PROCEDURA
026900970526     C*
027000970526     C                   ENDSR
027100970526     C*------------------------------------------------------------------------*
027200060907     C* F21GES - GESTIONE F21 ->
027300970526     C*------------------------------------------------------------------------*
027400970526     C     F21GES        BEGSR
027500970526     C*
027600970526     C*
027700970526     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028000970526     C*------------------------------------------------------------------------*
028100970526     C     F24GES        BEGSR
028200970526     C*
028300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028400970526     C*
028500970526     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700000121     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
028800000121     C*------------------------------------------------------------------------*
028900000120     C     INSERI        BEGSR
029000970526     C*
029100000121     C*
029200000121     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* MODIFI - OPERAZIONI DI MODIFICA
029500970526     C*------------------------------------------------------------------------*
029600970526     C     MODIFI        BEGSR
029700970526     C*
029800970526     C* CONTROLLA I CAMPI IMMESSI
029900970526     C                   EXSR      DECODI
030000970526     C*
030100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030300170914     C                   EXSR      LETLEG
030400970526     C*
030500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030700170914     C                   EXSR      IMPLEG                                       *IMPOSTA I CAMPI
030800170914     C                   MOVEL     *BLANKS       LEGaTB                         *ANNULLAMENTO
030900170914     C                   UPDATE    DPLEG000
031000160524     C*
031100160524     C* solo x EDP è possibile modificare l'indirizzo MAIL apertura Giacenza
031200160524     c                   if        *in91
031300171122     c     sav_imdnrr    chain     DPimd10f
031400171122     c                   if        %Found(DPimd10F)
031500160524     c                   movel     vidAGEml      imdMAIL
031600160524     C                   UPDATE    DPimdFIS
031700160524     c                   endif
031800160524     c                   endif
031900160524     C*
032000060907     C                   MOVEL     MSG(3)        De5MSG
032100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032200970526E   2C                   ENDIF
032300970526E   1C                   ENDIF
032400160524     C*
032500970526     C                   ENDSR
032600970526     C*------------------------------------------------------------------------*
032700060907     t* ANNULL - OPERAZIONI DI ANNULLAMENTO
032800970526     C*------------------------------------------------------------------------*
032900970526     C     ANNULL        BEGSR
033000970526     C*
033100970526     C*
033200970526     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
033500970526     C*------------------------------------------------------------------------*
033600970526     C     RIPRIS        BEGSR
033700970526     C*
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100170914     C* LETLEG - LEGGE IL RECORD PER L'AGGIORNAMENTO
034200970526     C*------------------------------------------------------------------------*
034300170914     C     LETLEG        BEGSR
034400970526     C*
034500170915     C                   movel     de5dep7       KCCdep7
034600060907     C                   z-add     de5dde        KCCdde
034700170914     C     KEYLEG        CHAIN     DPLEG11L                           9998
034800970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035000060907     C                   MOVEL     MSG(7)        De5MSG                         *MESSAGGIO
035100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035200970526E   1C                   ENDIF
035300970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035500060907     C                   MOVEL     MSG(6)        De5MSG                         *MESSAGGIO
035600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035700970526E   1C                   ENDIF
035800970526     C*
035900970526     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036200970526     C*------------------------------------------------------------------------*
036300970526     C     DECODI        BEGSR
036400150904     c                   setoff                                       44
036500060907     c                   clear                   vidnam1
036600060919     c                   clear                   vidadr1
036700060919     c                   clear                   vidptc
036800060907     c                   clear                   vidcty
036900060919     c****               clear                   vidnard
037000060907     c                   clear                   vidrag
037100060919     c                   clear                   vidvia
037200060919     c                   clear                   vidcap
037300060919     c                   clear                   vidcit
037400060919     c                   clear                   vidsta
037500060907     c* decodifico il depot solo se non scaduto
037600170918    1c                   if        LEGdsc >= datcor and osid1ver>0
037700170915     C                   clear                   cdpATB
037800170915     c     keycdp        chain     dpcdp11i
037900170915    2c                   if        %found(dpcdp11i)
038000170915     c                   movel     cdpname1      vidnam1
038100170915     c                   movel     cdpcity       vidcty
038200170915     c                   movel     cdpaddr1      vidadr1
038300060919     c                   movel     cdpptc        vidptc
038400060907    2c                   endif
038500060907    1c                   endif
038600060907     c* nazione
038700060919     c****               z-add     1             ktbkut
038800060919     c***                movel     '15'          ktbcod
038900060919     c***                movel(p)  vidnar        ktbkey
039000060919     c***  keytab        chain     tabel00f
039100060919    1c***                if        %found(tabel00f)
039200060919     c****               movel     tbluni        ds15
039300060919     c****               movel     §15des        vidnard
039400060919    1c****               endif
039500060907     c* codice cliente su piano dei conti
039600060907     c* se pieno lo controllo
039700060907    1c                   if        vidksc > *zeros
039800060907      * aggancio anagrafico
039900060907
040000060907     C                   clear                   TIBS69DS
040100060907     c                   z-add     vidksc        I69kac
040200060919     c                   z-add     vidksc        I69kin
040300060907     C                   call      'TIBS69R'
040400060907     C                   parm                    tibs69DS
040500060907     C                   parm                    DS_cnaco
040600060907     C                   parm                    DS_cnind
040700060907     C                   parm                    DS_cnclp
040800060907     C                   parm                    DS_fncls
040900060907    2c                   If         O69ERR =  '1'
041000060907     C                   eval      *IN60 = *on
041100060907     c                   eval      errl00 = '1'
041200060919     c                   movel     msg(9)        dsmsms
041300060907     c                   else
041400060907     c                   movel     ACOrag        vidrag
041500060919     c                   movel     indvia        vidvia
041600060919     c                   movel     indcap        vidcap
041700060919     c                   movel     indcit        vidcit
041800060919     c                   movel     indsta        vidsta
041900060907    2c                   Endif
042000060907    1c                   endif
042100060907     C*--------------
042200060907     C* data scadenza
042300060907     C*--------------
042400000121     C*
042500060907     C     *LIKE         DEFINE    VIDDSC        DSCVID
042600060907     c* controllo solo per profilo "EDP*"
042700060907   1ac                   if        *in91 = '1'
042800060907    1C                   IF        errl00 = '0'
042900060907     C                   Z-ADD     VIDDSC        WDA8
043000060907     C                   Z-ADD     *ZEROS        DA8W
043100060907     C                   EXSR      CHKDA8
043200060907     C                   Z-ADD     WDA8          VIDDSC
043300060907     C                   Z-ADD     DA8W          DSCVID
043400060907    2C     ERRL00        IFEQ      '1'
043500060907     C                   SETON                                        61
043600060907     C                   MOVEL     MSG(14)       DSMSMS
043700060907    2C                   ENDIF
043800060907    1C                   ENDIF
043900060907     C* SCADENZA > DECORRENZA
044000060907IF  1C     ERRL00        IFEQ      '0'
044100060907IF  2C     DSCVID        IFLT      de5dde
044200060907     C                   SETON                                        61
044300060907     C                   MOVEL     MSG(15)       DSMSMS
044400060907     C                   MOVEL     '1'           ERRL00
044500060907E   2C                   ENDIF
044600060907E   1C                   ENDIF
044700060922     C* scadenza non inferiore alla data corrente
044800060922IF  1C     ERRL00        IFEQ      '0'
044900060922IF  2C     DSCVID        IFLT      datcor
045000060922     C                   SETON                                        61
045100060922     C                   MOVEL     MSG(10)       DSMSMS
045200060922     C                   MOVEL     '1'           ERRL00
045300060922E   2C                   ENDIF
045400060922E   1C                   ENDIF
045500060907     C*---
045600060907     C* DECORRENZA + SCADENZA
045700060907     C*---
045800060907     C* ACCAVALLAMENTO DATI
045900060907IF  1C     ERRL00        IFEQ      '0'
046000170918     c                   movel     vidbun        viddep7
046100170918     c                   move      viddpc        viddep7
046200170915     C     viddep7       SETLL     dpLEG11l
046300060907DO  2C                   do        *hival
046400170915     C     viddep7       READE(N)  dpLEG11l
046500170914     c                   if        %eof(dpLEG11l)
046600060907     c                   leave
046700060907     c                   endif
046800060907IF  3C                   IF        (%subst(depop1:2:2) <> '06' and              *NON INSERIMENTO
046900170914     C                             de5dde <> LEGdde) or                         *RECORD IN LINEA
047000060907     c                             %subst(depop1:2:2) = '06'
047100170914IF  4C     LEGatb        IFEQ      *BLANKS
047200170914IF  5C     de5dde        IFGE      LEGdde                                       *DECORRENZA
047300170914     C     de5dde        ANDLE     LEGDSC
047400170914     C     DSCVID        ORGE      LEGDDE                                       *SCADENZA
047500170914     C     DSCVID        ANDLE     LEGDSC
047600170914     C     LEGDDE        ORGE      de5dde                                       *DECORRENZA
047700170914     C     LEGdSC        ANDLE     DSCVID
047800060907     C                   SETON                                        64
047900060907     C                   MOVEL     MSG(17)       DSMSMS
048000060907     C                   MOVEL     '1'           ERRL00
048100060907     C                   LEAVE
048200060907E   5C                   ENDIF
048300060907E   4C                   ENDIF
048400060907E   3C                   ENDIF
048500060907E   2C                   ENDDO
048600060907E   1C                   ENDIF
048700060907E  1ac                   ENDIF
048800000121     C*
048900150904     c                   if        vidnar <> *blank  and
049000150904     c                             vidnar <> vidsta
049100150904     c                   seton                                        44
049200150904     C                   end
049300000121     C*
049400150925     C*  controlla la LNP se presente su tabella LPD
049500150925     c                   if        vidlnp > 0
049600150925     C                   movel     'LPD'         TBECOD                         *TABELLA
049700150925     C                   movel(p)  vidlnp        TBEKE1                         *CHIAVE TABELLA
049800150925     C     KEYTBE        setll     tntbe01l
049900150925     c                   if        not %Equal(tntbe01l)
050000150925     C                   SETON                                        65
050100150925     C                   MOVEL     MSG(12)       DSMSMS
050200150925     C                   MOVEL     '1'           ERRL00
050300150925     C                   end
050400150925     C                   end
050500160524     C*
050600000121     C                   ENDSR
050700060907     C*------------------------------------------------------------------------*
050800060907     C* CHKDA8 - CONTROLLA LA DATA (8)
050900060907     C*------------------------------------------------------------------------*
051000060907     C     CHKDA8        BEGSR
051100060907     C*
051200060907     C     *LIKE         DEFINE    VIDDsc        WDA8
051300060907     C     *LIKE         DEFINE    VIDDsc        DA8W
051400060907     C*
051500060907     C                   Z-ADD     WDA8          G08DAT
051600060907     C                   Z-ADD     *ZEROS        G08INV
051700060907     C                   MOVEL     '0'           G08ERR
051800060907     C                   CALL      'XSRDA8'
051900060907     C                   PARM                    WLBDA8
052000060907     C                   Z-ADD     G08DAT        WDA8
052100060907     C                   Z-ADD     G08INV        DA8W
052200060907IF  1C     G08ERR        IFEQ      '1'
052300060907     C                   MOVEL     '1'           ERRL00
052400060907E   1C                   ENDIF
052500060907     C*
052600060907     C                   ENDSR
052700970526     C*------------------------------------------------------------------------*
052800970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
052900970526     C*------------------------------------------------------------------------*
053000970526     C     IREVID        BEGSR
053100970526     C* CAMPI
053200170918     C                   MOVE      LEGDPC        VIDdpc
053300170915     C                   MOVEL     LEGbun        VIDbun
053400171122     c                   clear                   viddBUN
053500171122     c                   move      vidbun        codBUN
053600171122     c     keyCBU        chain     dpCBU11i
053700171122     c                   if        %Found(dpCBU11i)
053800171122     c                   movel     CBUNAME       viddBUN
053900171122     c                   end
054000171122     C                   MOVE      LEGDPC        VIDdep7
054100171122     C                   MOVEL     LEGbun        VIDdep7
054200170914     c                   move      LEGdde        dataiso                        GG/MM/AAAA
054300060906     c                   move      dataiso       dataeur
054400060906     c                   move      dataeur       viddde
054500170914     C                   move      LEGnar        VIDnar
054600170914     C                   z-add     LEGksc        vidksc
054700170914     c                   move      LEGdsc        dataiso                        GG/MM/AAAA
054800060906     c                   move      dataiso       dataeur
054900060906     c                   move      dataeur       viddsc
055000170914     C                   MOVEL     LEGorma       VIDorma
055100170914     c                   move      LEGdtv        dataiso                        GG/MM/AAAA
055200060906     c                   move      dataiso       dataeur
055300060906     c                   move      dataeur       viddatv
055400170914     c                   z-add     LEGorv        vidorv
055500170914     c                   movel     LEGpru        vidpru
055600970526     C*
055700170914     c                   movel     LEGflo        dLEGflo
055800170914     c                   if        §LEGFLOLNP = *blank or
055900170914     c                             §LEGFLOLNP = *zeros
056000150924     c                   z-add     190           vidLNP
056100150924     C                   Else
056200150924     c                   move      *zeros        vidLNP
056300170914     c                   movel     §LEGFLOLNP    vidLNP
056400150924     C                   End
056500150924     C*
056600170914     c                   if        §LEGFLOGGI = *blank or
056700170914     c                             §LEGFLOGGI = *zeros
056800150924     c                   z-add     1             vidGGI
056900150924     C                   Else
057000150924     c                   move      *zeros        vidGGI
057100170914     c                   move      §LEGFLOGGI    vidGGI
057200150924     C                   End
057300160524     C*
057400160524     C*  controlla l'indirizzo MAIL presente sul DEPOT per l'invio delle
057500160524     C*  mail di apertura di Giacenza
057600171122     c     viddep7       setll     DPimd12L
057700171122     c     viddep7       reade     DPimd12L
057800160524     c                   clear                   sav_imdnrr
057900160524     c                   clear                   vidAGEml
058000171122     c                   dow       not %EoF(DPimd12L)
058100160524      **  se compreso nel periodo
058200160524    1c                   if        imddde <= datcor and imddsc >= datcor
058300160524     c                   z-add     IMD_nrr       sav_imdnrr
058400160524     c                   movel     IMDmail       vidAGEml
058500160524     c                   leave
058600160524     C                   end
058700160524      **
058800171122     c     viddep7       reade     DPimd12L
058900160524     c                   endDO
059000160524     C*
059100150924     C*
059200970526     C                   ENDSR
059300970526     C*------------------------------------------------------------------------*
059400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
059500970526     C*------------------------------------------------------------------------*
059600970526     C     AZZVID        BEGSR
059700970526     C* CAMPI
059800970526     C* DECODIFICHE
059900970526     C*
060000970526     C                   ENDSR
060100970526     C*------------------------------------------------------------------------*
060200970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
060300970526     C*------------------------------------------------------------------------*
060400170914     C     IMPLEG        BEGSR
060500970526     C*
060600170914     c                   if        vidksc <> LEGksc or dscvid <> LEGdsc
060700170914     c                             or vidorma <> LEGorma
060800170914     c                   z-add     datcor        LEGdtv
060900170914     c                   movel     wn14          LEGorv
061000170914     c                   movel     knmus         LEGpru
061100060907     c                   endif
061200060907     c
061300170914     C                   Z-ADD     VIDksc        LEGksc
061400060907     c                   if        *in91 = '1'
061500170914     C                   Z-ADD     dscvid        LEGdsc
061600170914     C                   movel     VIDorma       LEGorma
061700060907     c                   endif
061800150924     C*
061900170914     c                   move      *zeros        §LEGFLOLNP
062000170914     c                   move      vidLNP        §LEGFLOLNP
062100150924     C*
062200170914     c                   move      *zeros        §LEGFLOGGI
062300170914     c                   move      vidGGI        §LEGFLOGGI
062400150924     C*
062500170914     c                   eval      LEGflo =      dLEGflo
062600970526     C*
062700970526     C                   ENDSR
062800970526     C*------------------------------------------------------------------------*
062900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
063000970526     C*------------------------------------------------------------------------*
063100970526     C     IMPFUN        BEGSR
063200970603     C*
063300970526     C*
063400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
063500060906    1c                   if        %subst(depop1:2:2)= '05'
063600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
063700970526     C                   MOVEL     'N'           FUDATT
063800970526     C                   MOVEL     FUDRIG        FUD(6)
063900970526E   1C                   ENDIF
064000060907     C* SE gestione aggiunge la funzione di ricerca cod. cliente
064100060907    1c                   if        %subst(depop1:2:2)= '02'
064200060907     C                   MOVEL     FUD(7)        FUDRIG                         *AGGIORNAMENTO
064300060907     C                   MOVEL     'S'           FUDATT
064400060907     C                   MOVEL     FUDRIG        FUD(7)
064500060907E   1C                   ENDIF
064600970526     C*
064700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
064800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
064900970526     C                   EXSR      FUNGES
065000970526     C*
065100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
065200970526     C*---
065300060906     c                   setoff                                       9091
065400060906    1c                   if        %subst(depop1:2:2)= '05'
065500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
065600970526E   2C                   ENDIF
065700060906     c* se profilo edp rendo di I/O anche data scadenza e tipo ORM
065800060906     c                   if        %subst(knmus:1:3)='EDP'
065900060906     c                   seton                                        91
066000060906     c                   endif
066100970526     C*
066200970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
066300060906    1c                   if        %subst(depop1:2:2)<> '06'
066400170915     C                   movel     de5dep7       KCCdep7
066500060906     C                   MOVEL     De5dde        KCCdde
066600170914     C     KEYLEG        CHAIN(N)  DPLEG11L                           99
066700970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
066800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
066900970526     C                   MOVEL     MSG(6)        DSMSMS
067000970526X   2C                   ELSE
067100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
067200060906IF  3c                   if        %subst(depop1:2:2)= '05'
067300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
067400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
067500980128E   3C                   ENDIF
067600980128E   2C                   ENDIF
067700970526E   1C                   ENDIF
067800970526     C*
067900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
068000060906IF  1c                   if        %subst(depop1:2:2)= '02'
068100970526     C                   MOVEL     DEC(1)        VIDFUN
068200970526E   1C                   ENDIF
068300060906IF  1c                   if        %subst(depop1:2:2)= '05'
068400970526     C                   MOVEL     DEC(3)        VIDFUN
068500970526E   1C                   ENDIF
068600970526     C*
068700970526     C                   ENDSR
068800060907     C*--------------------------------------------------------------------------------------------*
068900060907     C* REPERISCE I DATI UTENTE
069000060907     C*--------------------------------------------------------------------------------------------*
069100060907     C     REPDATIUTE    BEGSR
069200060907     C*
069300060907     C* INIZIALIZZA VARIABILI DI WRK
069400060907     C                   CLEAR                   TIBS34DS
069500060907     C                   CLEAR                   AZUTEDS
069600060907     C                   CLEAR                   DDATIUTE
069700060907     C*
069800060907     C     *DTAARA       DEFINE    §azute        azuteds
069900060907     C     *DTAARA       DEFINE    §datiute      ddatiute
070000060907     C                   IN(E)     *DTAARA
070100060907if  1C                   IF        %Error
070200060907     c                   EVAL      I34Tla = 'L'
070300060907     C                   CALL      'TIBS34R'
070400060907     C                   PARM                    Tibs34Ds
070500060907     C                   IN        *DTAARA
070600060907e   1C                   ENDIF
070700060907     C*
070800060907     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
070900060907     C                   MOVEL(P)  RSUT          DSFIRS           20
071000060907     C*
071100060907     C                   ENDSR
071200970526     C*------------------------------------------------------------------------*
071300970526     C* *INZSR - OPERAZIONI INIZIALI
071400970526     C*------------------------------------------------------------------------*
071500970526     C     *INZSR        BEGSR
071600970526     C*---
071700970526     C* RICEVIMENTO PARAMETRI
071800970526     C*---
071900970526     C     *ENTRY        PLIST
072000970612     C                   PARM                    KPJBA
072100170919     C                   PARM                    tisie5ds
072200970526     C*
072300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
072400970526     C     XMSGPA        PLIST
072500970526     C                   PARM                    DSMSMS
072600970526     C                   PARM                    DSMSPG
072700970526     C                   PARM                    DSMSKE
072800970526     C                   PARM                    DSMSER
072900970526     C*
073000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
073100970526     C                   CALL      'XKEY'
073200970526     C                   PARM                    DSKEY
073300970526     C*---
073400970526     C* VARIABILI NON RIFERITE AL DATA BASE
073500970526     C*---
073600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073700060906     C     *LIKE         DEFINE    De5OP0        DEPOP0                         *DI DEPOSITO
073800060906     C     *LIKE         DEFINE    De5OP1        DEPOP1
073900060906     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
074000060906     C     *LIKE         DEFINE    De5OP1        WOP1
074100970526     C*---
074200970526     C* VARIABILI DI CONTROLLO
074300970526     C*---
074400970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
074500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
074800000120     C                   MOVEL     *BLANKS       WPGM             10            *ERRORE
074900970526     C*
075000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
075100170919     C                   MOVEL     'TISIE5R'     DSMSPG
075200170919     C                   MOVEL     'TISIE5R'     MSGQUE
075300970526     C                   MOVEL     *BLANKS       MSGKEY
075400970526     C                   MOVEL     *BLANKS       DSMSMS
075500970526     C*---
075600970526     C* VARIABILI RIFERITE AL DATA BASE
075700970526     C*---
075800970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
075900970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076000970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
076100170915     C     *LIKE         DEFINE    de5dep7       KCCdep7                        *DPCPC00F
076200170914     C     *LIKE         DEFINE    LEGdde        KCCdde
076300970526     C*---
076400970526     C* CHIAVI DI LETTURA
076500970526     C*---
076600970526     C* LETTURA TABEL00F - COMPLETA
076700970526     C     KEYTAB        KLIST
076800970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
076900970526     C                   KFLD                    KTBCOD                         *TABELLA
077000970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
077100150925     C* LETTURA TNTBE00F -
077200150925     C     KEYTBE        KLIST
077300150925     C                   KFLD                    TBECOD                         *TABELLA
077400150925     C                   KFLD                    TBEKE1                         *CHIAVE TABELLA
077500170914     C* LETTURA DPLEG11L - COMPLETA
077600170914     C     KEYLEG        KLIST
077700170915     C                   KFLD                    KCCdep7                        *VERSIONE
077800060906     C                   KFLD                    KCCdde                         *NAZIONE
077900171122     C* DPCDP11I
078000060906     C     KEYcdp        KLIST
078100170915     C                   KFLD                    cdpATB
078200170918     C                   KFLD                    osid1ver
078300170915     C                   KFLD                    LEGdep7
078400970526     C*
078500171122     C* DPCBU11I
078600171122     C     KEYcbu        KLIST
078700171122     C                   KFLD                    cdpATB
078800171122     C                   KFLD                    osid1ver
078900171122     C                   KFLD                    codBUN            3 0
079000171122     C*
079100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
079200060906     C                   MOVEL     De5OP0        DEPOP0                         *LIVELLO PROCEDURA
079300060906     C                   MOVEL     De5OP1        DEPOP1                         *LIVELLO PROGRAMMA
079400060907     C* REPERIMENTO DATI UTENTE
079500060907     C                   EXSR      REPDATIUTE
079600060918     C* CALCOLA LA DATA CORRENTE
079700060918     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
079800060918     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
079900060918     c                   move      wn8           dataeur                        GG/MM/AAAA
080000060918     c                   move      dataeur       dataiso
080100060918     c                   move      dataiso       datcor            8 0
080200170918     c* recupero l'ultima versione di cappario
080300170918     C                   clear                   ISID1DRI
080400170918     C                   clear                   OSID1VER
080500170918     C                   clear                   OSID1VERD
080600170918     C                   clear                   OSID1DDE
080700170918     C                   clear                   OSID1DSC
080800170918     C                   clear                   OSID1BCID
080900170918     c                   call      'TISID1R'
081000170918     C                   PARM                    ISID1DRI
081100170918     C                   PARM                    OSID1VER
081200170918     C                   PARM                    OSID1VERD
081300170918     C                   PARM                    OSID1DDE
081400170918     C                   PARM                    OSID1DSC
081500170918     C                   PARM                    OSID1BCID
081600970526     C*
081700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
081800970526     C                   EXSR      IMPFUN
081900970526     C*
082000970526     C                   ENDSR
082100970526     C*------------------------------------------------------------------------*
082200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
082300970526     C*------------------------------------------------------------------------*
082400970526     C     FUNGES        BEGSR
082500970526     C*
082600970526     C* PULISCO CAMPI DI LAVORO
082700970526     C                   Z-ADD     0             W                 5 0
082800970526     C*
082900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
083000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
083100970526     C     1             DO        24            I
083200970526     C                   MOVEL     FUD(I)        FUDRIG
083300970526     C*
083400970526     C* PULIZIA FUNZIONI PRECEDENTI
083500970526     C                   MOVEL     *BLANKS       FUV(I)
083600970526     C*
083700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
083800970526     C* (SALTA F24)
083900970526     C     FUDATT        IFEQ      'S'
084000970526     C     FUDFUN        ANDNE     '24'
084100970526     C                   ADD       1             W
084200970526     C                   MOVEL     FUDDES        FUV(W)
084300970526     C                   END
084400970526     C                   END                                                    DO WHILE
084500970526     C*
084600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
084700970526     C* ALTRIMENTI DISATTIVO F24
084800970526     C     1             DO        24            I
084900970526     C                   MOVEL     FUD(I)        FUDRIG
085000970526     C*
085100970526     C     FUDFUN        IFEQ      '24'
085200970526     C     W             IFGT      6
085300970526     C                   MOVEL     'S'           FUDATT
085400970526     C                   ELSE
085500970526     C                   MOVEL     *BLANKS       FUDATT
085600970526     C                   END                                                    W GT 8
085700970526     C                   MOVEL     FUDRIG        FUD(I)
085800970526     C                   END
085900970526     C                   END                                                    DO WHILE
086000970526     C*
086100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
086200970526     C     W             DIV       6             I
086300970526     C                   MVR                     Z
086400970526     C*
086500970526     C     Z             IFNE      0
086600970526     C     I             ADD       1             Z
086700970526     C                   ELSE
086800970526     C                   Z-ADD     I             Z                 5 0
086900970526     C                   END
087000970526     C*
087100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
087200970526     C                   ADD       1             FUNGRU
087300970526     C     FUNGRU        IFGT      Z
087400970526     C     FUNGRU        ORLT      1
087500970526     C                   Z-ADD     1             FUNGRU
087600970526     C                   END
087700970526     C*
087800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
087900970526     C     FUNGRU        IFEQ      1
088000970526     C                   MOVEL     FUV(1)        FUN001
088100970526     C                   MOVEL     FUV(2)        FUN002
088200970526     C                   MOVEL     FUV(3)        FUN003
088300970526     C                   MOVEL     FUV(4)        FUN004
088400970526     C                   MOVEL     FUV(5)        FUN005
088500970526     C                   MOVEL     FUV(6)        FUN006
088600970526     C                   END
088700970526     C*
088800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
088900970526     C     FUNGRU        IFEQ      2
089000970526     C                   MOVEL     FUV(7)        FUN001
089100970526     C                   MOVEL     FUV(8)        FUN002
089200970526     C                   MOVEL     FUV(9)        FUN003
089300970526     C                   MOVEL     FUV(10)       FUN004
089400970526     C                   MOVEL     FUV(11)       FUN005
089500970526     C                   MOVEL     FUV(12)       FUN006
089600970526     C                   END
089700970526     C*
089800970526     C* TERZO GRUPPO DI 6 FUNZIONI
089900970526     C     FUNGRU        IFEQ      3
090000970526     C                   MOVEL     FUV(13)       FUN001
090100970526     C                   MOVEL     FUV(14)       FUN002
090200970526     C                   MOVEL     FUV(15)       FUN003
090300970526     C                   MOVEL     FUV(16)       FUN004
090400970526     C                   MOVEL     FUV(17)       FUN005
090500970526     C                   MOVEL     FUV(18)       FUN006
090600970526     C                   END
090700970526     C*
090800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
090900970526     C     FUNGRU        IFEQ      4
091000970526     C                   MOVEL     FUV(19)       FUN001
091100970526     C                   MOVEL     FUV(20)       FUN002
091200970526     C                   MOVEL     FUV(21)       FUN003
091300970526     C                   MOVEL     FUV(22)       FUN004
091400970526     C                   MOVEL     *BLANKS       FUN005
091500970526     C                   MOVEL     *BLANKS       FUN006
091600970526     C                   END
091700970526     C*
091800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
091900970526     C                   MOVEL     *BLANKS       FUN007
092000970526     C                   MOVEL     *BLANKS       FUN008
092100970526     C*
092200970526     C     1             DO        24            I
092300970526     C                   MOVEL     FUD(I)        FUDRIG
092400970526     C     FUDFUN        IFEQ      '24'
092500970526     C     FUDATT        ANDEQ     'S'
092600970526     C                   MOVEL     FUDDES        FUN008
092700970526     C                   END
092800970526     C                   END                                                    DO
092900970526     C*
093000970526     C                   ENDSR
093100970526     C*------------------------------------------------------------------------*
093200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
093300970526     C*------------------------------------------------------------------------*
093400970526     C     FUNCON        BEGSR
093500970526     C*
093600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093700970526     C                   EXSR      FUNVAR
093800970526     C*
093900970526     C* PULISCO CAMPI DI LAVORO
094000970526     C                   Z-ADD     0             I                 5 0
094100970526     C                   Z-ADD     0             W                 5 0
094200970526     C*
094300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
094400970526     C     FUNKEY        IFNE      *BLANKS
094500970526     C     1             DO        24            I
094600970526     C                   MOVEL     FUD(I)        FUDRIG
094700970526     C*
094800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094900970526     C     FUNKEY        IFEQ      FUDFUN
095000970526     C     FUDATT        ANDEQ     'S'
095100970526     C                   Z-ADD     I             W
095200970526     C                   END
095300970526     C                   END                                                    DO WHILE
095400970526     C*
095500970526     C* FUNZIONE NON ATTIVA (ERRORE)
095600970526     C     W             IFEQ      0
095700970526     C*
095800970526     C* EMISSIONE MESSAGGIO DI ERRORE
095900970526     C                   MOVEL     MSG(1)        DSMSMS
096000970526     C*
096100970526     C                   END                                                    W=0
096200970526     C                   END                                                    FUNKEY NE *BLANKS
096300970526     C*
096400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
096500970526     C     W             IFNE      0
096600970526     C     FUNKEY        OREQ      *BLANKS
096700970526     C     FUNKEY        IFEQ      *BLANKS
096800970526     C                   EXSR      ENTGES
096900970526     C                   END
097000970526     C     FUNKEY        IFEQ      '03'
097100970526     C                   EXSR      F03GES
097200970526     C                   END
097300060907     C     FUNKEY        IFEQ      '07'
097400060907     C                   EXSR      f07ges
097500060907     C                   END
097600970526     C     FUNKEY        IFEQ      '06'
097700970526     C                   EXSR      F06GES
097800970526     C                   END
097900970526     C     FUNKEY        IFEQ      '12'
098000970526     C                   EXSR      F12GES
098100970526     C                   END
098200970526     C     FUNKEY        IFEQ      '24'
098300970526     C                   EXSR      F24GES
098400970526     C                   END
098500970526     C                   END
098600970526     C*
098700970526     C                   ENDSR
098800970526     C*------------------------------------------------------------------------*
098900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
099000970526     C*------------------------------------------------------------------------*
099100970526     C     FUNVAR        BEGSR
099200970526     C*
099300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
099400970526     C*
099500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099600970526     C     KEY           IFEQ      ENTER
099700970526     C                   MOVEL     *BLANKS       FUNKEY
099800970526     C                   END
099900970526     C     KEY           IFEQ      F1
100000970526     C                   MOVEL     '01'          FUNKEY
100100970526     C                   END
100200970526     C     KEY           IFEQ      F2
100300970526     C                   MOVEL     '02'          FUNKEY
100400970526     C                   END
100500970526     C     KEY           IFEQ      F3
100600970526     C                   MOVEL     '03'          FUNKEY
100700970526     C                   END
100800970526     C     KEY           IFEQ      F4
100900970526     C                   MOVEL     '04'          FUNKEY
101000970526     C                   END
101100970526     C     KEY           IFEQ      F5
101200970526     C                   MOVEL     '05'          FUNKEY
101300970526     C                   END
101400970526     C     KEY           IFEQ      F6
101500970526     C                   MOVEL     '06'          FUNKEY
101600970526     C                   END
101700970526     C     KEY           IFEQ      F7
101800970526     C                   MOVEL     '07'          FUNKEY
101900970526     C                   END
102000970526     C     KEY           IFEQ      F8
102100970526     C                   MOVEL     '08'          FUNKEY
102200970526     C                   END
102300970526     C     KEY           IFEQ      F9
102400970526     C                   MOVEL     '09'          FUNKEY
102500970526     C                   END
102600970526     C     KEY           IFEQ      F10
102700970526     C                   MOVEL     '10'          FUNKEY
102800970526     C                   END
102900970526     C     KEY           IFEQ      F11
103000970526     C                   MOVEL     '11'          FUNKEY
103100970526     C                   END
103200970526     C     KEY           IFEQ      F12
103300970526     C                   MOVEL     '12'          FUNKEY
103400970526     C                   END
103500970526     C     KEY           IFEQ      F13
103600970526     C                   MOVEL     '13'          FUNKEY
103700970526     C                   END
103800970526     C     KEY           IFEQ      F14
103900970526     C                   MOVEL     '14'          FUNKEY
104000970526     C                   END
104100970526     C     KEY           IFEQ      F15
104200970526     C                   MOVEL     '15'          FUNKEY
104300970526     C                   END
104400970526     C     KEY           IFEQ      F16
104500970526     C                   MOVEL     '16'          FUNKEY
104600970526     C                   END
104700970526     C     KEY           IFEQ      F17
104800970526     C                   MOVEL     '17'          FUNKEY
104900970526     C                   END
105000970526     C     KEY           IFEQ      F18
105100970526     C                   MOVEL     '18'          FUNKEY
105200970526     C                   END
105300970526     C     KEY           IFEQ      F19
105400970526     C                   MOVEL     '19'          FUNKEY
105500970526     C                   END
105600970526     C     KEY           IFEQ      F20
105700970526     C                   MOVEL     '20'          FUNKEY
105800970526     C                   END
105900970526     C     KEY           IFEQ      F21
106000970526     C                   MOVEL     '21'          FUNKEY
106100970526     C                   END
106200970526     C     KEY           IFEQ      F22
106300970526     C                   MOVEL     '22'          FUNKEY
106400970526     C                   END
106500970526     C     KEY           IFEQ      F23
106600970526     C                   MOVEL     '23'          FUNKEY
106700970526     C                   END
106800970526     C     KEY           IFEQ      F24
106900970526     C                   MOVEL     '24'          FUNKEY
107000970526     C                   END
107100970526     C     KEY           IFEQ      ROLLUP
107200970526     C                   MOVEL     'RU'          FUNKEY
107300970526     C                   END
107400970526     C     KEY           IFEQ      HELP
107500970526     C                   MOVEL     'HP'          FUNKEY
107600970526     C                   END
107700970526     C                   ENDSR
107800970526     C*------------------------------------------------------------------------*
107900970526     C* NOTE COSTRUZIONE SCHIERE
108000970526     C*------------------------------------------------------------------------*
108100970526     C*
108200970526     C* -MSG-MESSAGGI
108300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
108400970526     C* (NON TOCCARE MSG,1 E MSG,2)
108500970526     C*
108600970526     C* -CMD-COMANDI
108700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108800970526     C*
108900970526     C* -OPD-OPZIONI
109000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
109100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
109200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
109300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
109400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
109600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
109800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
109900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
110000970526     C*
110100970526     C* -FUD-FUNZIONI
110200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
110300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
110400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
110500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
110600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
110700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
110800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
110900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
111000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
111100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
111200970526     O*------------------------------------------------------------------------*
111300970526** DEC - DECODIFICHE
111400970526    MODIFICA                                                      1
111500970526  ANNULLAMENTO                                                    2
111600970526VISUALIZZAZIONE                                                   3
111700970526   RIPRISTINO                                                     4
111800970526  INSERIMENTO                                                     5
111900970526** MSG - MESSAGGI
112000060906TISIE5R- Tasto funzionale non gestito                                          1
112100060906TISIE5R- Inserimento effettuato                                                2
112200060906TISIE5R- Modifica effettuata                                                   3
112300060906TISIE5R- Annullamento effettuato                                               4
112400060906TISIE5R- Ripristino effettuato                                                 5
112500060906TISIE5R- Richiesta funzione per record inesistente                             6
112600060906TISIE5R- Record momentaneamente allocato: riprovare più tardi                  7
112700060906TISIE5R- Richiesta funzione per record esistente                               8
112800060919TISIE5R- Codice inesistente su Piano dei Conti                                 9
112900060922TISIE5R- Il record non può scadere in un giorno già trascorso                 10
113000060919TISIE5R- 11                                                                   11
113100150925TISIE5R- Linea NON corretta per DPD import Bolle                              12
113200060919TISIE5R- 13                                                                   13
113300060919TISIE5R- Data errata                                                          14
113400060919TISIE5R- La data scadenza deve essere maggiore della decorrenza               15
113500060906TISIE5R-                                                                      16
113600060919TISIE5R- Il record si sovrappone ad una decorrenza/scadenza già presente      17
113700060906TISIE5R-                                                                      18
113800060906TISIE5R-                                                                      19
113900060919TISIE5R- 20                                                                   20
114000060906TISIE5R-                                                                      21
114100060906TISIE5R- 22
114200060906TISIE5R- 23
114300060906TISIE5R- 24
114400060906TISIE5R-                                                                      25
114500060919TISIE5R- 26                                                                   26
114600060906TISIE5R-                                                                      27
114700060906TISIE5R-                                                                      28
114800060906TISIE5R-                                                                      29
114900060906TISIE5R-                                                                      30
115000970526** FUD - FUNZIONI
115100970526                   -        (S/N)=N                        funz.01¬
115200970530                   -        (S/N)=N                        funz.02¬
115300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
115400970526                   -        (S/N)=N                        funz.04¬
115500970526                   -        (S/N)=N                        funz.05¬
115600970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
115700060907F07=Ricerca Cliente-        (S/N)=N                        funz.07¬
115800000120                   -        (S/N)=N                        funz.08¬
115900000120                   -        (S/N)=N                        funz.09¬
116000000120                   -        (S/N)=N                        funz.10¬
116100970526                   -        (S/N)=N                        funz.11¬
116200970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
116300970526                   -        (S/N)=N                        funz.13¬
116400000120                   -        (S/N)=N                        funz.14¬
116500970526                   -        (S/N)=N                        funz.15¬
116600970526                   -        (S/N)=N                        funz.16¬
116700970526                   -        (S/N)=N                        funz.17¬
116800970526                   -        (S/N)=N                        funz.18¬
116900970526                   -        (S/N)=N                        funz.19¬
117000970526                   -        (S/N)=N                        funz.20¬
117100970526                   -        (S/N)=N                        funz.21¬
117200970526                   -        (S/N)=N                        funz.22¬
117300970526                   -        (S/N)=N                        funz.23¬
117400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
