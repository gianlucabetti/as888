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
001200171214     FDPimd10F  uF a E             DISK    rename(DPIMD100:dpimdFIS)
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
031500171214     c                   if        vidAGEml = *blank
031600171214     C                   DELETE    DPimdFIS
031700171214     c                   else
031800160524     c                   movel     vidAGEml      imdMAIL
031900160524     C                   UPDATE    DPimdFIS
032000171214     c                   end
032100171214     c                   else
032200171214      *
032300171214     c                   movel     legDPC        IMDDPC
032400171214     c                   z-add     010101        IMDDDE
032500171214     c                   z-add     99991231      IMDDSC
032600171214     c                   movel     legDEP7       IMDDEP7
032700171214      *
032800171214     c                   movel     vidAGEml      imdMAIL
032900171214     C                   WRITE     DPimdFIS
033000171214     c                   endif
033100160524     c                   endif
033200160524     C*
033300060907     C                   MOVEL     MSG(3)        De5MSG
033400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033500970526E   2C                   ENDIF
033600970526E   1C                   ENDIF
033700160524     C*
033800970526     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000060907     t* ANNULL - OPERAZIONI DI ANNULLAMENTO
034100970526     C*------------------------------------------------------------------------*
034200970526     C     ANNULL        BEGSR
034300970526     C*
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     RIPRIS        BEGSR
035000970526     C*
035100970526     C*
035200970526     C                   ENDSR
035300970526     C*------------------------------------------------------------------------*
035400170914     C* LETLEG - LEGGE IL RECORD PER L'AGGIORNAMENTO
035500970526     C*------------------------------------------------------------------------*
035600170914     C     LETLEG        BEGSR
035700970526     C*
035800170915     C                   movel     de5dep7       KCCdep7
035900060907     C                   z-add     de5dde        KCCdde
036000170914     C     KEYLEG        CHAIN     DPLEG11L                           9998
036100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
036200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
036300060907     C                   MOVEL     MSG(7)        De5MSG                         *MESSAGGIO
036400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036500970526E   1C                   ENDIF
036600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
036700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
036800060907     C                   MOVEL     MSG(6)        De5MSG                         *MESSAGGIO
036900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
037000970526E   1C                   ENDIF
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
037500970526     C*------------------------------------------------------------------------*
037600970526     C     DECODI        BEGSR
037700150904     c                   setoff                                       44
037800060907     c                   clear                   vidnam1
037900060919     c                   clear                   vidadr1
038000060919     c                   clear                   vidptc
038100060907     c                   clear                   vidcty
038200060919     c****               clear                   vidnard
038300060907     c                   clear                   vidrag
038400060919     c                   clear                   vidvia
038500060919     c                   clear                   vidcap
038600060919     c                   clear                   vidcit
038700060919     c                   clear                   vidsta
038800060907     c* decodifico il depot solo se non scaduto
038900170918    1c                   if        LEGdsc >= datcor and osid1ver>0
039000170915     C                   clear                   cdpATB
039100170915     c     keycdp        chain     dpcdp11i
039200170915    2c                   if        %found(dpcdp11i)
039300170915     c                   movel     cdpname1      vidnam1
039400170915     c                   movel     cdpcity       vidcty
039500170915     c                   movel     cdpaddr1      vidadr1
039600060919     c                   movel     cdpptc        vidptc
039700060907    2c                   endif
039800060907    1c                   endif
039900060907     c* nazione
040000060919     c****               z-add     1             ktbkut
040100060919     c***                movel     '15'          ktbcod
040200060919     c***                movel(p)  vidnar        ktbkey
040300060919     c***  keytab        chain     tabel00f
040400060919    1c***                if        %found(tabel00f)
040500060919     c****               movel     tbluni        ds15
040600060919     c****               movel     §15des        vidnard
040700060919    1c****               endif
040800060907     c* codice cliente su piano dei conti
040900060907     c* se pieno lo controllo
041000060907    1c                   if        vidksc > *zeros
041100060907      * aggancio anagrafico
041200060907
041300060907     C                   clear                   TIBS69DS
041400060907     c                   z-add     vidksc        I69kac
041500060919     c                   z-add     vidksc        I69kin
041600060907     C                   call      'TIBS69R'
041700060907     C                   parm                    tibs69DS
041800060907     C                   parm                    DS_cnaco
041900060907     C                   parm                    DS_cnind
042000060907     C                   parm                    DS_cnclp
042100060907     C                   parm                    DS_fncls
042200060907    2c                   If         O69ERR =  '1'
042300060907     C                   eval      *IN60 = *on
042400060907     c                   eval      errl00 = '1'
042500060919     c                   movel     msg(9)        dsmsms
042600060907     c                   else
042700060907     c                   movel     ACOrag        vidrag
042800060919     c                   movel     indvia        vidvia
042900060919     c                   movel     indcap        vidcap
043000060919     c                   movel     indcit        vidcit
043100060919     c                   movel     indsta        vidsta
043200060907    2c                   Endif
043300060907    1c                   endif
043400060907     C*--------------
043500060907     C* data scadenza
043600060907     C*--------------
043700000121     C*
043800060907     C     *LIKE         DEFINE    VIDDSC        DSCVID
043900060907     c* controllo solo per profilo "EDP*"
044000060907   1ac                   if        *in91 = '1'
044100060907    1C                   IF        errl00 = '0'
044200060907     C                   Z-ADD     VIDDSC        WDA8
044300060907     C                   Z-ADD     *ZEROS        DA8W
044400060907     C                   EXSR      CHKDA8
044500060907     C                   Z-ADD     WDA8          VIDDSC
044600060907     C                   Z-ADD     DA8W          DSCVID
044700060907    2C     ERRL00        IFEQ      '1'
044800060907     C                   SETON                                        61
044900060907     C                   MOVEL     MSG(14)       DSMSMS
045000060907    2C                   ENDIF
045100060907    1C                   ENDIF
045200060907     C* SCADENZA > DECORRENZA
045300060907IF  1C     ERRL00        IFEQ      '0'
045400060907IF  2C     DSCVID        IFLT      de5dde
045500060907     C                   SETON                                        61
045600060907     C                   MOVEL     MSG(15)       DSMSMS
045700060907     C                   MOVEL     '1'           ERRL00
045800060907E   2C                   ENDIF
045900060907E   1C                   ENDIF
046000060922     C* scadenza non inferiore alla data corrente
046100060922IF  1C     ERRL00        IFEQ      '0'
046200060922IF  2C     DSCVID        IFLT      datcor
046300060922     C                   SETON                                        61
046400060922     C                   MOVEL     MSG(10)       DSMSMS
046500060922     C                   MOVEL     '1'           ERRL00
046600060922E   2C                   ENDIF
046700060922E   1C                   ENDIF
046800060907     C*---
046900060907     C* DECORRENZA + SCADENZA
047000060907     C*---
047100060907     C* ACCAVALLAMENTO DATI
047200060907IF  1C     ERRL00        IFEQ      '0'
047300170918     c                   movel     vidbun        viddep7
047400170918     c                   move      viddpc        viddep7
047500170915     C     viddep7       SETLL     dpLEG11l
047600060907DO  2C                   do        *hival
047700170915     C     viddep7       READE(N)  dpLEG11l
047800170914     c                   if        %eof(dpLEG11l)
047900060907     c                   leave
048000060907     c                   endif
048100060907IF  3C                   IF        (%subst(depop1:2:2) <> '06' and              *NON INSERIMENTO
048200170914     C                             de5dde <> LEGdde) or                         *RECORD IN LINEA
048300060907     c                             %subst(depop1:2:2) = '06'
048400170914IF  4C     LEGatb        IFEQ      *BLANKS
048500170914IF  5C     de5dde        IFGE      LEGdde                                       *DECORRENZA
048600170914     C     de5dde        ANDLE     LEGDSC
048700170914     C     DSCVID        ORGE      LEGDDE                                       *SCADENZA
048800170914     C     DSCVID        ANDLE     LEGDSC
048900170914     C     LEGDDE        ORGE      de5dde                                       *DECORRENZA
049000170914     C     LEGdSC        ANDLE     DSCVID
049100060907     C                   SETON                                        64
049200060907     C                   MOVEL     MSG(17)       DSMSMS
049300060907     C                   MOVEL     '1'           ERRL00
049400060907     C                   LEAVE
049500060907E   5C                   ENDIF
049600060907E   4C                   ENDIF
049700060907E   3C                   ENDIF
049800060907E   2C                   ENDDO
049900060907E   1C                   ENDIF
050000060907E  1ac                   ENDIF
050100000121     C*
050200150904     c                   if        vidnar <> *blank  and
050300150904     c                             vidnar <> vidsta
050400150904     c                   seton                                        44
050500150904     C                   end
050600000121     C*
050700150925     C*  controlla la LNP se presente su tabella LPD
050800150925     c                   if        vidlnp > 0
050900150925     C                   movel     'LPD'         TBECOD                         *TABELLA
051000150925     C                   movel(p)  vidlnp        TBEKE1                         *CHIAVE TABELLA
051100150925     C     KEYTBE        setll     tntbe01l
051200150925     c                   if        not %Equal(tntbe01l)
051300150925     C                   SETON                                        65
051400150925     C                   MOVEL     MSG(12)       DSMSMS
051500150925     C                   MOVEL     '1'           ERRL00
051600150925     C                   end
051700150925     C                   end
051800160524     C*
051900000121     C                   ENDSR
052000060907     C*------------------------------------------------------------------------*
052100060907     C* CHKDA8 - CONTROLLA LA DATA (8)
052200060907     C*------------------------------------------------------------------------*
052300060907     C     CHKDA8        BEGSR
052400060907     C*
052500060907     C     *LIKE         DEFINE    VIDDsc        WDA8
052600060907     C     *LIKE         DEFINE    VIDDsc        DA8W
052700060907     C*
052800060907     C                   Z-ADD     WDA8          G08DAT
052900060907     C                   Z-ADD     *ZEROS        G08INV
053000060907     C                   MOVEL     '0'           G08ERR
053100060907     C                   CALL      'XSRDA8'
053200060907     C                   PARM                    WLBDA8
053300060907     C                   Z-ADD     G08DAT        WDA8
053400060907     C                   Z-ADD     G08INV        DA8W
053500060907IF  1C     G08ERR        IFEQ      '1'
053600060907     C                   MOVEL     '1'           ERRL00
053700060907E   1C                   ENDIF
053800060907     C*
053900060907     C                   ENDSR
054000970526     C*------------------------------------------------------------------------*
054100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
054200970526     C*------------------------------------------------------------------------*
054300970526     C     IREVID        BEGSR
054400970526     C* CAMPI
054500170918     C                   MOVE      LEGDPC        VIDdpc
054600170915     C                   MOVEL     LEGbun        VIDbun
054700171122     c                   clear                   viddBUN
054800171122     c                   move      vidbun        codBUN
054900171122     c     keyCBU        chain     dpCBU11i
055000171122     c                   if        %Found(dpCBU11i)
055100171122     c                   movel     CBUNAME       viddBUN
055200171122     c                   end
055300171122     C                   MOVE      LEGDPC        VIDdep7
055400171122     C                   MOVEL     LEGbun        VIDdep7
055500170914     c                   move      LEGdde        dataiso                        GG/MM/AAAA
055600060906     c                   move      dataiso       dataeur
055700060906     c                   move      dataeur       viddde
055800170914     C                   move      LEGnar        VIDnar
055900170914     C                   z-add     LEGksc        vidksc
056000170914     c                   move      LEGdsc        dataiso                        GG/MM/AAAA
056100060906     c                   move      dataiso       dataeur
056200060906     c                   move      dataeur       viddsc
056300170914     C                   MOVEL     LEGorma       VIDorma
056400170914     c                   move      LEGdtv        dataiso                        GG/MM/AAAA
056500060906     c                   move      dataiso       dataeur
056600060906     c                   move      dataeur       viddatv
056700170914     c                   z-add     LEGorv        vidorv
056800170914     c                   movel     LEGpru        vidpru
056900970526     C*
057000170914     c                   movel     LEGflo        dLEGflo
057100170914     c                   if        §LEGFLOLNP = *blank or
057200170914     c                             §LEGFLOLNP = *zeros
057300150924     c                   z-add     190           vidLNP
057400150924     C                   Else
057500150924     c                   move      *zeros        vidLNP
057600170914     c                   movel     §LEGFLOLNP    vidLNP
057700150924     C                   End
057800150924     C*
057900170914     c                   if        §LEGFLOGGI = *blank or
058000170914     c                             §LEGFLOGGI = *zeros
058100150924     c                   z-add     1             vidGGI
058200150924     C                   Else
058300150924     c                   move      *zeros        vidGGI
058400170914     c                   move      §LEGFLOGGI    vidGGI
058500150924     C                   End
058600160524     C*
058700160524     C*  controlla l'indirizzo MAIL presente sul DEPOT per l'invio delle
058800160524     C*  mail di apertura di Giacenza
058900171122     c     viddep7       setll     DPimd12L
059000171122     c     viddep7       reade     DPimd12L
059100160524     c                   clear                   sav_imdnrr
059200160524     c                   clear                   vidAGEml
059300171122     c                   dow       not %EoF(DPimd12L)
059400160524      **  se compreso nel periodo
059500160524    1c                   if        imddde <= datcor and imddsc >= datcor
059600160524     c                   z-add     IMD_nrr       sav_imdnrr
059700160524     c                   movel     IMDmail       vidAGEml
059800160524     c                   leave
059900160524     C                   end
060000160524      **
060100171122     c     viddep7       reade     DPimd12L
060200160524     c                   endDO
060300160524     C*
060400150924     C*
060500970526     C                   ENDSR
060600970526     C*------------------------------------------------------------------------*
060700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
060800970526     C*------------------------------------------------------------------------*
060900970526     C     AZZVID        BEGSR
061000970526     C* CAMPI
061100970526     C* DECODIFICHE
061200970526     C*
061300970526     C                   ENDSR
061400970526     C*------------------------------------------------------------------------*
061500970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
061600970526     C*------------------------------------------------------------------------*
061700170914     C     IMPLEG        BEGSR
061800970526     C*
061900170914     c                   if        vidksc <> LEGksc or dscvid <> LEGdsc
062000170914     c                             or vidorma <> LEGorma
062100170914     c                   z-add     datcor        LEGdtv
062200170914     c                   movel     wn14          LEGorv
062300170914     c                   movel     knmus         LEGpru
062400060907     c                   endif
062500060907     c
062600170914     C                   Z-ADD     VIDksc        LEGksc
062700060907     c                   if        *in91 = '1'
062800170914     C                   Z-ADD     dscvid        LEGdsc
062900170914     C                   movel     VIDorma       LEGorma
063000060907     c                   endif
063100150924     C*
063200170914     c                   move      *zeros        §LEGFLOLNP
063300170914     c                   move      vidLNP        §LEGFLOLNP
063400150924     C*
063500170914     c                   move      *zeros        §LEGFLOGGI
063600170914     c                   move      vidGGI        §LEGFLOGGI
063700150924     C*
063800170914     c                   eval      LEGflo =      dLEGflo
063900970526     C*
064000970526     C                   ENDSR
064100970526     C*------------------------------------------------------------------------*
064200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
064300970526     C*------------------------------------------------------------------------*
064400970526     C     IMPFUN        BEGSR
064500970603     C*
064600970526     C*
064700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
064800060906    1c                   if        %subst(depop1:2:2)= '05'
064900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
065000970526     C                   MOVEL     'N'           FUDATT
065100970526     C                   MOVEL     FUDRIG        FUD(6)
065200970526E   1C                   ENDIF
065300060907     C* SE gestione aggiunge la funzione di ricerca cod. cliente
065400060907    1c                   if        %subst(depop1:2:2)= '02'
065500060907     C                   MOVEL     FUD(7)        FUDRIG                         *AGGIORNAMENTO
065600060907     C                   MOVEL     'S'           FUDATT
065700060907     C                   MOVEL     FUDRIG        FUD(7)
065800060907E   1C                   ENDIF
065900970526     C*
066000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
066100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
066200970526     C                   EXSR      FUNGES
066300970526     C*
066400970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
066500970526     C*---
066600171212     c                   setoff                                       909192
066700060906    1c                   if        %subst(depop1:2:2)= '05'
066800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066900970526E   2C                   ENDIF
067000060906     c* se profilo edp rendo di I/O anche data scadenza e tipo ORM
067100060906     c                   if        %subst(knmus:1:3)='EDP'
067200060906     c                   seton                                        91
067300060906     c                   endif
067400970526     C*
067500171212     C*   Abilitato Solfrini a modificare l'indirizzo MAIL per l'apertura
067600171212     C*   della giacenza come utenti di sede
067700171212     c                   if        DUTPOU = 046
067800171212     c                   seton                                        92
067900171212     c                   endif
068000171212     C*
068100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
068200060906    1c                   if        %subst(depop1:2:2)<> '06'
068300170915     C                   movel     de5dep7       KCCdep7
068400060906     C                   MOVEL     De5dde        KCCdde
068500170914     C     KEYLEG        CHAIN(N)  DPLEG11L                           99
068600970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
068700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
068800970526     C                   MOVEL     MSG(6)        DSMSMS
068900970526X   2C                   ELSE
069000970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
069100060906IF  3c                   if        %subst(depop1:2:2)= '05'
069200980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
069300980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
069400980128E   3C                   ENDIF
069500980128E   2C                   ENDIF
069600970526E   1C                   ENDIF
069700970526     C*
069800970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
069900060906IF  1c                   if        %subst(depop1:2:2)= '02'
070000970526     C                   MOVEL     DEC(1)        VIDFUN
070100970526E   1C                   ENDIF
070200060906IF  1c                   if        %subst(depop1:2:2)= '05'
070300970526     C                   MOVEL     DEC(3)        VIDFUN
070400970526E   1C                   ENDIF
070500970526     C*
070600970526     C                   ENDSR
070700060907     C*--------------------------------------------------------------------------------------------*
070800060907     C* REPERISCE I DATI UTENTE
070900060907     C*--------------------------------------------------------------------------------------------*
071000060907     C     REPDATIUTE    BEGSR
071100060907     C*
071200060907     C* INIZIALIZZA VARIABILI DI WRK
071300060907     C                   CLEAR                   TIBS34DS
071400060907     C                   CLEAR                   AZUTEDS
071500060907     C                   CLEAR                   DDATIUTE
071600060907     C*
071700060907     C     *DTAARA       DEFINE    §azute        azuteds
071800060907     C     *DTAARA       DEFINE    §datiute      ddatiute
071900060907     C                   IN(E)     *DTAARA
072000060907if  1C                   IF        %Error
072100060907     c                   EVAL      I34Tla = 'L'
072200060907     C                   CALL      'TIBS34R'
072300060907     C                   PARM                    Tibs34Ds
072400060907     C                   IN        *DTAARA
072500060907e   1C                   ENDIF
072600060907     C*
072700060907     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
072800060907     C                   MOVEL(P)  RSUT          DSFIRS           20
072900060907     C*
073000060907     C                   ENDSR
073100970526     C*------------------------------------------------------------------------*
073200970526     C* *INZSR - OPERAZIONI INIZIALI
073300970526     C*------------------------------------------------------------------------*
073400970526     C     *INZSR        BEGSR
073500970526     C*---
073600970526     C* RICEVIMENTO PARAMETRI
073700970526     C*---
073800970526     C     *ENTRY        PLIST
073900970612     C                   PARM                    KPJBA
074000170919     C                   PARM                    tisie5ds
074100970526     C*
074200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
074300970526     C     XMSGPA        PLIST
074400970526     C                   PARM                    DSMSMS
074500970526     C                   PARM                    DSMSPG
074600970526     C                   PARM                    DSMSKE
074700970526     C                   PARM                    DSMSER
074800970526     C*
074900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
075000970526     C                   CALL      'XKEY'
075100970526     C                   PARM                    DSKEY
075200970526     C*---
075300970526     C* VARIABILI NON RIFERITE AL DATA BASE
075400970526     C*---
075500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
075600060906     C     *LIKE         DEFINE    De5OP0        DEPOP0                         *DI DEPOSITO
075700060906     C     *LIKE         DEFINE    De5OP1        DEPOP1
075800060906     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
075900060906     C     *LIKE         DEFINE    De5OP1        WOP1
076000970526     C*---
076100970526     C* VARIABILI DI CONTROLLO
076200970526     C*---
076300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
076400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
076500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
076600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
076700000120     C                   MOVEL     *BLANKS       WPGM             10            *ERRORE
076800970526     C*
076900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
077000170919     C                   MOVEL     'TISIE5R'     DSMSPG
077100170919     C                   MOVEL     'TISIE5R'     MSGQUE
077200970526     C                   MOVEL     *BLANKS       MSGKEY
077300970526     C                   MOVEL     *BLANKS       DSMSMS
077400970526     C*---
077500970526     C* VARIABILI RIFERITE AL DATA BASE
077600970526     C*---
077700970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
077800970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
077900970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
078000170915     C     *LIKE         DEFINE    de5dep7       KCCdep7                        *DPCPC00F
078100170914     C     *LIKE         DEFINE    LEGdde        KCCdde
078200970526     C*---
078300970526     C* CHIAVI DI LETTURA
078400970526     C*---
078500970526     C* LETTURA TABEL00F - COMPLETA
078600970526     C     KEYTAB        KLIST
078700970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
078800970526     C                   KFLD                    KTBCOD                         *TABELLA
078900970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
079000150925     C* LETTURA TNTBE00F -
079100150925     C     KEYTBE        KLIST
079200150925     C                   KFLD                    TBECOD                         *TABELLA
079300150925     C                   KFLD                    TBEKE1                         *CHIAVE TABELLA
079400170914     C* LETTURA DPLEG11L - COMPLETA
079500170914     C     KEYLEG        KLIST
079600170915     C                   KFLD                    KCCdep7                        *VERSIONE
079700060906     C                   KFLD                    KCCdde                         *NAZIONE
079800171122     C* DPCDP11I
079900060906     C     KEYcdp        KLIST
080000170915     C                   KFLD                    cdpATB
080100170918     C                   KFLD                    osid1ver
080200170915     C                   KFLD                    LEGdep7
080300970526     C*
080400171122     C* DPCBU11I
080500171122     C     KEYcbu        KLIST
080600171122     C                   KFLD                    cdpATB
080700171122     C                   KFLD                    osid1ver
080800171122     C                   KFLD                    codBUN            3 0
080900171122     C*
081000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
081100060906     C                   MOVEL     De5OP0        DEPOP0                         *LIVELLO PROCEDURA
081200060906     C                   MOVEL     De5OP1        DEPOP1                         *LIVELLO PROGRAMMA
081300060907     C* REPERIMENTO DATI UTENTE
081400060907     C                   EXSR      REPDATIUTE
081500060918     C* CALCOLA LA DATA CORRENTE
081600060918     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
081700060918     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
081800060918     c                   move      wn8           dataeur                        GG/MM/AAAA
081900060918     c                   move      dataeur       dataiso
082000060918     c                   move      dataiso       datcor            8 0
082100170918     c* recupero l'ultima versione di cappario
082200170918     C                   clear                   ISID1DRI
082300170918     C                   clear                   OSID1VER
082400170918     C                   clear                   OSID1VERD
082500170918     C                   clear                   OSID1DDE
082600170918     C                   clear                   OSID1DSC
082700170918     C                   clear                   OSID1BCID
082800170918     c                   call      'TISID1R'
082900170918     C                   PARM                    ISID1DRI
083000170918     C                   PARM                    OSID1VER
083100170918     C                   PARM                    OSID1VERD
083200170918     C                   PARM                    OSID1DDE
083300170918     C                   PARM                    OSID1DSC
083400170918     C                   PARM                    OSID1BCID
083500970526     C*
083600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
083700970526     C                   EXSR      IMPFUN
083800970526     C*
083900970526     C                   ENDSR
084000970526     C*------------------------------------------------------------------------*
084100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
084200970526     C*------------------------------------------------------------------------*
084300970526     C     FUNGES        BEGSR
084400970526     C*
084500970526     C* PULISCO CAMPI DI LAVORO
084600970526     C                   Z-ADD     0             W                 5 0
084700970526     C*
084800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
084900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
085000970526     C     1             DO        24            I
085100970526     C                   MOVEL     FUD(I)        FUDRIG
085200970526     C*
085300970526     C* PULIZIA FUNZIONI PRECEDENTI
085400970526     C                   MOVEL     *BLANKS       FUV(I)
085500970526     C*
085600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
085700970526     C* (SALTA F24)
085800970526     C     FUDATT        IFEQ      'S'
085900970526     C     FUDFUN        ANDNE     '24'
086000970526     C                   ADD       1             W
086100970526     C                   MOVEL     FUDDES        FUV(W)
086200970526     C                   END
086300970526     C                   END                                                    DO WHILE
086400970526     C*
086500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
086600970526     C* ALTRIMENTI DISATTIVO F24
086700970526     C     1             DO        24            I
086800970526     C                   MOVEL     FUD(I)        FUDRIG
086900970526     C*
087000970526     C     FUDFUN        IFEQ      '24'
087100970526     C     W             IFGT      6
087200970526     C                   MOVEL     'S'           FUDATT
087300970526     C                   ELSE
087400970526     C                   MOVEL     *BLANKS       FUDATT
087500970526     C                   END                                                    W GT 8
087600970526     C                   MOVEL     FUDRIG        FUD(I)
087700970526     C                   END
087800970526     C                   END                                                    DO WHILE
087900970526     C*
088000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
088100970526     C     W             DIV       6             I
088200970526     C                   MVR                     Z
088300970526     C*
088400970526     C     Z             IFNE      0
088500970526     C     I             ADD       1             Z
088600970526     C                   ELSE
088700970526     C                   Z-ADD     I             Z                 5 0
088800970526     C                   END
088900970526     C*
089000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
089100970526     C                   ADD       1             FUNGRU
089200970526     C     FUNGRU        IFGT      Z
089300970526     C     FUNGRU        ORLT      1
089400970526     C                   Z-ADD     1             FUNGRU
089500970526     C                   END
089600970526     C*
089700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
089800970526     C     FUNGRU        IFEQ      1
089900970526     C                   MOVEL     FUV(1)        FUN001
090000970526     C                   MOVEL     FUV(2)        FUN002
090100970526     C                   MOVEL     FUV(3)        FUN003
090200970526     C                   MOVEL     FUV(4)        FUN004
090300970526     C                   MOVEL     FUV(5)        FUN005
090400970526     C                   MOVEL     FUV(6)        FUN006
090500970526     C                   END
090600970526     C*
090700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
090800970526     C     FUNGRU        IFEQ      2
090900970526     C                   MOVEL     FUV(7)        FUN001
091000970526     C                   MOVEL     FUV(8)        FUN002
091100970526     C                   MOVEL     FUV(9)        FUN003
091200970526     C                   MOVEL     FUV(10)       FUN004
091300970526     C                   MOVEL     FUV(11)       FUN005
091400970526     C                   MOVEL     FUV(12)       FUN006
091500970526     C                   END
091600970526     C*
091700970526     C* TERZO GRUPPO DI 6 FUNZIONI
091800970526     C     FUNGRU        IFEQ      3
091900970526     C                   MOVEL     FUV(13)       FUN001
092000970526     C                   MOVEL     FUV(14)       FUN002
092100970526     C                   MOVEL     FUV(15)       FUN003
092200970526     C                   MOVEL     FUV(16)       FUN004
092300970526     C                   MOVEL     FUV(17)       FUN005
092400970526     C                   MOVEL     FUV(18)       FUN006
092500970526     C                   END
092600970526     C*
092700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
092800970526     C     FUNGRU        IFEQ      4
092900970526     C                   MOVEL     FUV(19)       FUN001
093000970526     C                   MOVEL     FUV(20)       FUN002
093100970526     C                   MOVEL     FUV(21)       FUN003
093200970526     C                   MOVEL     FUV(22)       FUN004
093300970526     C                   MOVEL     *BLANKS       FUN005
093400970526     C                   MOVEL     *BLANKS       FUN006
093500970526     C                   END
093600970526     C*
093700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
093800970526     C                   MOVEL     *BLANKS       FUN007
093900970526     C                   MOVEL     *BLANKS       FUN008
094000970526     C*
094100970526     C     1             DO        24            I
094200970526     C                   MOVEL     FUD(I)        FUDRIG
094300970526     C     FUDFUN        IFEQ      '24'
094400970526     C     FUDATT        ANDEQ     'S'
094500970526     C                   MOVEL     FUDDES        FUN008
094600970526     C                   END
094700970526     C                   END                                                    DO
094800970526     C*
094900970526     C                   ENDSR
095000970526     C*------------------------------------------------------------------------*
095100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
095200970526     C*------------------------------------------------------------------------*
095300970526     C     FUNCON        BEGSR
095400970526     C*
095500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
095600970526     C                   EXSR      FUNVAR
095700970526     C*
095800970526     C* PULISCO CAMPI DI LAVORO
095900970526     C                   Z-ADD     0             I                 5 0
096000970526     C                   Z-ADD     0             W                 5 0
096100970526     C*
096200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
096300970526     C     FUNKEY        IFNE      *BLANKS
096400970526     C     1             DO        24            I
096500970526     C                   MOVEL     FUD(I)        FUDRIG
096600970526     C*
096700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
096800970526     C     FUNKEY        IFEQ      FUDFUN
096900970526     C     FUDATT        ANDEQ     'S'
097000970526     C                   Z-ADD     I             W
097100970526     C                   END
097200970526     C                   END                                                    DO WHILE
097300970526     C*
097400970526     C* FUNZIONE NON ATTIVA (ERRORE)
097500970526     C     W             IFEQ      0
097600970526     C*
097700970526     C* EMISSIONE MESSAGGIO DI ERRORE
097800970526     C                   MOVEL     MSG(1)        DSMSMS
097900970526     C*
098000970526     C                   END                                                    W=0
098100970526     C                   END                                                    FUNKEY NE *BLANKS
098200970526     C*
098300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
098400970526     C     W             IFNE      0
098500970526     C     FUNKEY        OREQ      *BLANKS
098600970526     C     FUNKEY        IFEQ      *BLANKS
098700970526     C                   EXSR      ENTGES
098800970526     C                   END
098900970526     C     FUNKEY        IFEQ      '03'
099000970526     C                   EXSR      F03GES
099100970526     C                   END
099200060907     C     FUNKEY        IFEQ      '07'
099300060907     C                   EXSR      f07ges
099400060907     C                   END
099500970526     C     FUNKEY        IFEQ      '06'
099600970526     C                   EXSR      F06GES
099700970526     C                   END
099800970526     C     FUNKEY        IFEQ      '12'
099900970526     C                   EXSR      F12GES
100000970526     C                   END
100100970526     C     FUNKEY        IFEQ      '24'
100200970526     C                   EXSR      F24GES
100300970526     C                   END
100400970526     C                   END
100500970526     C*
100600970526     C                   ENDSR
100700970526     C*------------------------------------------------------------------------*
100800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
100900970526     C*------------------------------------------------------------------------*
101000970526     C     FUNVAR        BEGSR
101100970526     C*
101200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
101300970526     C*
101400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
101500970526     C     KEY           IFEQ      ENTER
101600970526     C                   MOVEL     *BLANKS       FUNKEY
101700970526     C                   END
101800970526     C     KEY           IFEQ      F1
101900970526     C                   MOVEL     '01'          FUNKEY
102000970526     C                   END
102100970526     C     KEY           IFEQ      F2
102200970526     C                   MOVEL     '02'          FUNKEY
102300970526     C                   END
102400970526     C     KEY           IFEQ      F3
102500970526     C                   MOVEL     '03'          FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F4
102800970526     C                   MOVEL     '04'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F5
103100970526     C                   MOVEL     '05'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F6
103400970526     C                   MOVEL     '06'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F7
103700970526     C                   MOVEL     '07'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F8
104000970526     C                   MOVEL     '08'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F9
104300970526     C                   MOVEL     '09'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F10
104600970526     C                   MOVEL     '10'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F11
104900970526     C                   MOVEL     '11'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F12
105200970526     C                   MOVEL     '12'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F13
105500970526     C                   MOVEL     '13'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F14
105800970526     C                   MOVEL     '14'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F15
106100970526     C                   MOVEL     '15'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F16
106400970526     C                   MOVEL     '16'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F17
106700970526     C                   MOVEL     '17'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F18
107000970526     C                   MOVEL     '18'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F19
107300970526     C                   MOVEL     '19'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F20
107600970526     C                   MOVEL     '20'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F21
107900970526     C                   MOVEL     '21'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F22
108200970526     C                   MOVEL     '22'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F23
108500970526     C                   MOVEL     '23'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F24
108800970526     C                   MOVEL     '24'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      ROLLUP
109100970526     C                   MOVEL     'RU'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      HELP
109400970526     C                   MOVEL     'HP'          FUNKEY
109500970526     C                   END
109600970526     C                   ENDSR
109700970526     C*------------------------------------------------------------------------*
109800970526     C* NOTE COSTRUZIONE SCHIERE
109900970526     C*------------------------------------------------------------------------*
110000970526     C*
110100970526     C* -MSG-MESSAGGI
110200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
110300970526     C* (NON TOCCARE MSG,1 E MSG,2)
110400970526     C*
110500970526     C* -CMD-COMANDI
110600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
110700970526     C*
110800970526     C* -OPD-OPZIONI
110900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
111000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
111100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
111200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
111300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
111400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
111500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
111600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
111700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
111800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
111900970526     C*
112000970526     C* -FUD-FUNZIONI
112100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
112200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
112300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
112400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
112500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
112600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
112700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
112800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
112900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
113000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
113100970526     O*------------------------------------------------------------------------*
113200970526** DEC - DECODIFICHE
113300970526    MODIFICA                                                      1
113400970526  ANNULLAMENTO                                                    2
113500970526VISUALIZZAZIONE                                                   3
113600970526   RIPRISTINO                                                     4
113700970526  INSERIMENTO                                                     5
113800970526** MSG - MESSAGGI
113900060906TISIE5R- Tasto funzionale non gestito                                          1
114000060906TISIE5R- Inserimento effettuato                                                2
114100060906TISIE5R- Modifica effettuata                                                   3
114200060906TISIE5R- Annullamento effettuato                                               4
114300060906TISIE5R- Ripristino effettuato                                                 5
114400060906TISIE5R- Richiesta funzione per record inesistente                             6
114500060906TISIE5R- Record momentaneamente allocato: riprovare più tardi                  7
114600060906TISIE5R- Richiesta funzione per record esistente                               8
114700060919TISIE5R- Codice inesistente su Piano dei Conti                                 9
114800060922TISIE5R- Il record non può scadere in un giorno già trascorso                 10
114900060919TISIE5R- 11                                                                   11
115000150925TISIE5R- Linea NON corretta per DPD import Bolle                              12
115100060919TISIE5R- 13                                                                   13
115200060919TISIE5R- Data errata                                                          14
115300060919TISIE5R- La data scadenza deve essere maggiore della decorrenza               15
115400060906TISIE5R-                                                                      16
115500060919TISIE5R- Il record si sovrappone ad una decorrenza/scadenza già presente      17
115600060906TISIE5R-                                                                      18
115700060906TISIE5R-                                                                      19
115800060919TISIE5R- 20                                                                   20
115900060906TISIE5R-                                                                      21
116000060906TISIE5R- 22
116100060906TISIE5R- 23
116200060906TISIE5R- 24
116300060906TISIE5R-                                                                      25
116400060919TISIE5R- 26                                                                   26
116500060906TISIE5R-                                                                      27
116600060906TISIE5R-                                                                      28
116700060906TISIE5R-                                                                      29
116800060906TISIE5R-                                                                      30
116900970526** FUD - FUNZIONI
117000970526                   -        (S/N)=N                        funz.01¬
117100970530                   -        (S/N)=N                        funz.02¬
117200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
117300970526                   -        (S/N)=N                        funz.04¬
117400970526                   -        (S/N)=N                        funz.05¬
117500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
117600060907F07=Ricerca Cliente-        (S/N)=N                        funz.07¬
117700000120                   -        (S/N)=N                        funz.08¬
117800000120                   -        (S/N)=N                        funz.09¬
117900000120                   -        (S/N)=N                        funz.10¬
118000970526                   -        (S/N)=N                        funz.11¬
118100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
118200970526                   -        (S/N)=N                        funz.13¬
118300000120                   -        (S/N)=N                        funz.14¬
118400970526                   -        (S/N)=N                        funz.15¬
118500970526                   -        (S/N)=N                        funz.16¬
118600970526                   -        (S/N)=N                        funz.17¬
118700970526                   -        (S/N)=N                        funz.18¬
118800970526                   -        (S/N)=N                        funz.19¬
118900970526                   -        (S/N)=N                        funz.20¬
119000970526                   -        (S/N)=N                        funz.21¬
119100970526                   -        (S/N)=N                        funz.22¬
119200970526                   -        (S/N)=N                        funz.23¬
119300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
