000100970526     H*------------------------------------------------------------------------*
000200170914     H* Legami Depot DPD/LEG - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170914     FTISIe15d1 CF   E             WORKSTN INFDS(DEVDS1)
000900060906     F                                     SFILE(SIE5DMS:MSGULT)
001000170914     FDPLEG11L  UF   E           K DISK
001100160524     FDPimd01L  iF   E           K DISK    INFDS(IMDDS)
001200160524     FDPimd00F  uF   E             DISK    rename(DPIMD000:dpimdFIS)
001300170915     Fdpcdp11i  IF   E           K DISK
001400060906     Ftabel00f  IF   E           K DISK
001500150925     Ftntbe01l  IF   E           K DISK
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900170918     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970526     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700160524     D*-------------
005800160524     d IMDds           ds
005900160524     d  imd_nrr              397    400b 0
006000160524      *
006100160524     d  sav_imdnrr     s                   like(IMD_nrr)
006200060907     D*------------
006300060907     D* DS REPERIMENTO DATI UTENTE
006400060907     D*------------
006500060907     D TIBS34DS      E DS                                                       *Profili utente
006600060907     D DDATIUTE      E DS                                                       *Dati utente
006700060907     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006800170914     D DLEGFLO       E DS
006900060907     D*------------------
007000060907     D* DS "XSRDA8" - CONTROLLA DATA (8)
007100060907     D*------------------
007200060907     D WLBDA8          DS                  INZ
007300060907     D  G08DAT                 1      8  0
007400060907     D  G08INV                 9     16  0
007500060907     D  G08ERR                17     17
007600060907     D  G08TGI                18     22  0
007700970526     D*------------
007800970526     D* DS "XMSG" - GESTIONE MESSAGGI
007900970526     D*------------
008000970526     D DSMSG         E DS
008100970526     D*------------------
008200970526     D* DS NAZIONI
008300970526     D*------------------
008400970526     D DS15          E DS
008500970526     D*------------------
008600970526     D* DS DI PROCEDURA
008700970526     D*------------
008800170914     D tisie15ds     E DS
008900060906     D*------------
009000060907      * - Ds per Anagrafiche Clienti
009100060907     d TIBS69DS      E DS
009200060907     d DS_cnaco      E DS                  extname(CNACO00F)
009300060907     d DS_cnind      E DS                  extname(CNIND00F)
009400060907     d DS_cnclp      E DS                  extname(CNCLP00F)
009500060907     d DS_fncls      E DS                  extname(FNCLS00F)
009600970526     D*------------
009700970526     D* ARCHITETTURA
009800970526     D*------------
009900970526     D KPJBA         E DS
010000060906     d
010100060906     d dataiso         s               d   datfmt(*iso)
010200060906     d dataeur         s               d   datfmt(*eur)
010300970526     C*------------------------------------------------------------------------*
010400970526     C* MAIN LINES
010500970526     C*------------------------------------------------------------------------*
010600970526     C*
010700970526     C* NON CI SONO ERRORI IN ENTRATA
010800970526IF  1C     ERRL00        IFEQ      '0'
010900970526     C*
011000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011400970526     C*
011500970526     C* EMETTE FORMATO VIDEO
011600970526     C                   EXSR      BUTFMR
011700970526     C*
011800970526     C* GESTIONE TASTI FUNZIONALI
011900970526     C                   EXSR      FUNCON
012000970526E   2C                   ENDDO
012100970526     C*
012200970526     C* CI SONO ERRORI IN ENTRATA
012300970527X   1C                   ELSE
012400060906     C                   MOVEL     '1'           De5ERR
012500060906     C                   MOVEL     DSMSMS        De5MSG
012600970526E   1C                   ENDIF
012700970526     C* FINE PGM
012800060907     C                   eval      I69TLA  = 'C'
012900060907     C                   call      'TIBS69R'
013000060907     C                   parm                    TIBS69DS
013100060907     c*
013200970526     C                   SETON                                        LR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     BUTFMR        BEGSR
013700970526     C*
013800970526     C* EMETTE LA TESTATA
013900060906     C                   WRITE     SIe5D01
014000970526     C*
014100970526     C* SPEDISCE EVENTUALE MESSAGGIO
014200970526     C                   EXSR      SNDMSG
014300970526     C*
014400970526     C* EMETTE LE FUNZIONI
014500060906     C                   WRITE     SIe5D02
014600970526     C*
014700000121     C* EMETTE IL VIDEO ED ESEGUE LA DECODIFICA
014800000120     C                   EXSR      DECODI
014900060906     C                   EXFMT     SIe5D03
015000970526     C*
015100970526     C* REIMPOSTA FLAG DI ERRORE
015200970526     C                   MOVEL     '0'           ERRL00
015300150925     c                   clear                   dsmsms
015400000121     C* REIMPOSTA INDICATORI DI ERRORE
015500150925     C                   SETOFF                                       606165
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     SNDMSG        BEGSR
016200970526     C*
016300970526     C* PULISCE IL SFILE MESSAGGI
016400970526     C                   MOVEL     '1'           *IN40
016500060906     C                   WRITE     SIe5DMC
016600970526     C                   MOVEL     '0'           *IN40
016700970526     C*
016800970526     C* VISUALIZZA IL MESSAGGIO
016900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017000970526     C                   CALL      'XMSG'        XMSGPA
017100970526IF  1C     DSMSER        IFEQ      *ZEROS
017200970526     C                   MOVEL     DSMSKE        MSGKEY
017300970526E   1C                   ENDIF
017400970526     C                   Z-ADD     1             MSGULT
017500060906     C                   WRITE     SIe5DMS
017600060906     C                   WRITE     SIe5DMC
017700970526     C*
017800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017900970526     C                   MOVEL     *BLANKS       DSMSMS
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* ENTGES - GESTIONE ENTER -> INVIO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     ENTGES        BEGSR
018600970526     C*
018700970526     C* CONTROLLA I CAMPI IMMESSI
018800970526     C                   EXSR      DECODI
018900970526     C*
019000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
019100980128IF  1C     DEPOP1        IFEQ      'O05'
019200980128     C                   MOVEL     '1'           FINL00
019300980128E   1C                   ENDIF
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F03GES - GESTIONE F03 -> FINE
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F03GES        BEGSR
020000970526     C*
020100970526     C* IMPOSTA "PREMUTO F03"
020200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
020300060906     C                   MOVEL     '1'           De5F03                         *DI PROCEDURA
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F06GES        BEGSR
021000970526     C*
021100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021200970526IF  1C     DEPOP1        IFEQ      'F06'
021300970526     C                   EXSR      INSERI
021400970526E   1C                   ENDIF
021500970526IF  1C     DEPOP1        IFEQ      'O02'
021600970526     C                   EXSR      MODIFI
021700970526E   1C                   ENDIF
021800970526IF  1C     DEPOP1        IFEQ      'O04'
021900970526     C                   EXSR      ANNULL
022000970526E   1C                   ENDIF
022100970526IF  1C     DEPOP1        IFEQ      'O07'
022200970526     C                   EXSR      RIPRIS
022300970526E   1C                   ENDIF
022400970526     C*
022500970526     C                   ENDSR
022600060907     C*------------------------------------------------------------------------*
022700060907     C* F07GES - GESTIONE F07 -> reicerca codice cliente
022800060907     C*------------------------------------------------------------------------*
022900060907     C     F07GEs        BEGSR
023000060907     C*
023100060907     C                   MOVEL     rsut          PAXdut                         Descrizione Azienda
023200060907     C                   Z-ADD     3             PAXkut                         tipo estrazione
023300060907     C                   Z-ADD     9             PAXsta                         9=Escludo Annullati
023400060907     C                   Z-ADD     DUTKCI        PAXkcc                         Capoconto Clienti
023500060907     c                   movel(P)  Vidrag        PAXrag
023600060907     C                   z-add     1             PAXnum
023700060907      *
023800060907     C                   CALL      'XALFA3BR'
023900060907     C                   PARM                    PAXDUT           30
024000060907     C                   PARM                    PAXkut            1 0
024100060907     C                   PARM                    PAXrag           48
024200060907     C                   PARM                    PAXkcc            4 0
024300060907     C                   PARM                    PAXsta            1 0
024400060907     C                   PARM      *blanks       PAXflr           90
024500060907     C                   PARM      *blanks       PAXdit            3
024600060907     C                   PARM                    PAXnum            2 0
024700060907     C                   PARM                    PAXkcm           80
024800060907     C                   PARM                    PAXksm          140
024900060907     C                   PARM                    PAXkdm           60
025000060907      *
025100060907      * NON TROVATO O PREMUTO CMD12
025200060907    2C                   If        paxsta > -1
025300060907     C                   movel     PAXksm        w007a             7
025400060907     c                   move      w007a         vidksc
025500060907     C                   movel     PAXrag        Vidrag
025600060907     c                   endif
025700060907     c
025800060907     C*
025900060907     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* F12GES - GESTIONE F12 -> RITORNO
026200970526     C*------------------------------------------------------------------------*
026300970526     C     F12GES        BEGSR
026400970526     C*
026500970526     C* IMPOSTA "PREMUTO F12"
026600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026700060906     C                   MOVEL     '1'           de5F12                         *DI PROCEDURA
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100060907     C* F21GES - GESTIONE F21 ->
027200970526     C*------------------------------------------------------------------------*
027300970526     C     F21GES        BEGSR
027400970526     C*
027500970526     C*
027600970526     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027900970526     C*------------------------------------------------------------------------*
028000970526     C     F24GES        BEGSR
028100970526     C*
028200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600000121     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
028700000121     C*------------------------------------------------------------------------*
028800000120     C     INSERI        BEGSR
028900970526     C*
029000000121     C*
029100000121     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* MODIFI - OPERAZIONI DI MODIFICA
029400970526     C*------------------------------------------------------------------------*
029500970526     C     MODIFI        BEGSR
029600970526     C*
029700970526     C* CONTROLLA I CAMPI IMMESSI
029800970526     C                   EXSR      DECODI
029900970526     C*
030000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030200170914     C                   EXSR      LETLEG
030300970526     C*
030400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030600170914     C                   EXSR      IMPLEG                                       *IMPOSTA I CAMPI
030700170914     C                   MOVEL     *BLANKS       LEGaTB                         *ANNULLAMENTO
030800170914     C                   UPDATE    DPLEG000
030900160524     C*
031000160524     C* solo x EDP è possibile modificare l'indirizzo MAIL apertura Giacenza
031100160524     c                   if        *in91
031200160524     c     sav_imdnrr    chain     DPimd00f
031300160524     c                   if        %Found(DPimd00F)
031400160524     c                   movel     vidAGEml      imdMAIL
031500160524     C                   UPDATE    DPimdFIS
031600160524     c                   endif
031700160524     c                   endif
031800160524     C*
031900060907     C                   MOVEL     MSG(3)        De5MSG
032000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032100970526E   2C                   ENDIF
032200970526E   1C                   ENDIF
032300160524     C*
032400970526     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600060907     t* ANNULL - OPERAZIONI DI ANNULLAMENTO
032700970526     C*------------------------------------------------------------------------*
032800970526     C     ANNULL        BEGSR
032900970526     C*
033000970526     C*
033100970526     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
033400970526     C*------------------------------------------------------------------------*
033500970526     C     RIPRIS        BEGSR
033600970526     C*
033700970526     C*
033800970526     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000170914     C* LETLEG - LEGGE IL RECORD PER L'AGGIORNAMENTO
034100970526     C*------------------------------------------------------------------------*
034200170914     C     LETLEG        BEGSR
034300970526     C*
034400170915     C                   movel     de5dep7       KCCdep7
034500060907     C                   z-add     de5dde        KCCdde
034600170914     C     KEYLEG        CHAIN     DPLEG11L                           9998
034700970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034900060907     C                   MOVEL     MSG(7)        De5MSG                         *MESSAGGIO
035000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035100970526E   1C                   ENDIF
035200970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035400060907     C                   MOVEL     MSG(6)        De5MSG                         *MESSAGGIO
035500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035600970526E   1C                   ENDIF
035700970526     C*
035800970526     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036100970526     C*------------------------------------------------------------------------*
036200970526     C     DECODI        BEGSR
036300150904     c                   setoff                                       44
036400060907     c                   clear                   vidnam1
036500060919     c                   clear                   vidadr1
036600060919     c                   clear                   vidptc
036700060907     c                   clear                   vidcty
036800060919     c****               clear                   vidnard
036900060907     c                   clear                   vidrag
037000060919     c                   clear                   vidvia
037100060919     c                   clear                   vidcap
037200060919     c                   clear                   vidcit
037300060919     c                   clear                   vidsta
037400060907     c* decodifico il depot solo se non scaduto
037500170918    1c                   if        LEGdsc >= datcor and osid1ver>0
037600170915     C                   clear                   cdpATB
037700170915     c     keycdp        chain     dpcdp11i
037800170915    2c                   if        %found(dpcdp11i)
037900170915     c                   movel     cdpname1      vidnam1
038000170915     c                   movel     cdpcity       vidcty
038100170915     c                   movel     cdpaddr1      vidadr1
038200060919     c                   movel     cdpptc        vidptc
038300060907    2c                   endif
038400060907    1c                   endif
038500060907     c* nazione
038600060919     c****               z-add     1             ktbkut
038700060919     c***                movel     '15'          ktbcod
038800060919     c***                movel(p)  vidnar        ktbkey
038900060919     c***  keytab        chain     tabel00f
039000060919    1c***                if        %found(tabel00f)
039100060919     c****               movel     tbluni        ds15
039200060919     c****               movel     §15des        vidnard
039300060919    1c****               endif
039400060907     c* codice cliente su piano dei conti
039500060907     c* se pieno lo controllo
039600060907    1c                   if        vidksc > *zeros
039700060907      * aggancio anagrafico
039800060907
039900060907     C                   clear                   TIBS69DS
040000060907     c                   z-add     vidksc        I69kac
040100060919     c                   z-add     vidksc        I69kin
040200060907     C                   call      'TIBS69R'
040300060907     C                   parm                    tibs69DS
040400060907     C                   parm                    DS_cnaco
040500060907     C                   parm                    DS_cnind
040600060907     C                   parm                    DS_cnclp
040700060907     C                   parm                    DS_fncls
040800060907    2c                   If         O69ERR =  '1'
040900060907     C                   eval      *IN60 = *on
041000060907     c                   eval      errl00 = '1'
041100060919     c                   movel     msg(9)        dsmsms
041200060907     c                   else
041300060907     c                   movel     ACOrag        vidrag
041400060919     c                   movel     indvia        vidvia
041500060919     c                   movel     indcap        vidcap
041600060919     c                   movel     indcit        vidcit
041700060919     c                   movel     indsta        vidsta
041800060907    2c                   Endif
041900060907    1c                   endif
042000060907     C*--------------
042100060907     C* data scadenza
042200060907     C*--------------
042300000121     C*
042400060907     C     *LIKE         DEFINE    VIDDSC        DSCVID
042500060907     c* controllo solo per profilo "EDP*"
042600060907   1ac                   if        *in91 = '1'
042700060907    1C                   IF        errl00 = '0'
042800060907     C                   Z-ADD     VIDDSC        WDA8
042900060907     C                   Z-ADD     *ZEROS        DA8W
043000060907     C                   EXSR      CHKDA8
043100060907     C                   Z-ADD     WDA8          VIDDSC
043200060907     C                   Z-ADD     DA8W          DSCVID
043300060907    2C     ERRL00        IFEQ      '1'
043400060907     C                   SETON                                        61
043500060907     C                   MOVEL     MSG(14)       DSMSMS
043600060907    2C                   ENDIF
043700060907    1C                   ENDIF
043800060907     C* SCADENZA > DECORRENZA
043900060907IF  1C     ERRL00        IFEQ      '0'
044000060907IF  2C     DSCVID        IFLT      de5dde
044100060907     C                   SETON                                        61
044200060907     C                   MOVEL     MSG(15)       DSMSMS
044300060907     C                   MOVEL     '1'           ERRL00
044400060907E   2C                   ENDIF
044500060907E   1C                   ENDIF
044600060922     C* scadenza non inferiore alla data corrente
044700060922IF  1C     ERRL00        IFEQ      '0'
044800060922IF  2C     DSCVID        IFLT      datcor
044900060922     C                   SETON                                        61
045000060922     C                   MOVEL     MSG(10)       DSMSMS
045100060922     C                   MOVEL     '1'           ERRL00
045200060922E   2C                   ENDIF
045300060922E   1C                   ENDIF
045400060907     C*---
045500060907     C* DECORRENZA + SCADENZA
045600060907     C*---
045700060907     C* ACCAVALLAMENTO DATI
045800060907IF  1C     ERRL00        IFEQ      '0'
045900170918     c                   movel     vidbun        viddep7
046000170918     c                   move      viddpc        viddep7
046100170915     C     viddep7       SETLL     dpLEG11l
046200060907DO  2C                   do        *hival
046300170915     C     viddep7       READE(N)  dpLEG11l
046400170914     c                   if        %eof(dpLEG11l)
046500060907     c                   leave
046600060907     c                   endif
046700060907IF  3C                   IF        (%subst(depop1:2:2) <> '06' and              *NON INSERIMENTO
046800170914     C                             de5dde <> LEGdde) or                         *RECORD IN LINEA
046900060907     c                             %subst(depop1:2:2) = '06'
047000170914IF  4C     LEGatb        IFEQ      *BLANKS
047100170914IF  5C     de5dde        IFGE      LEGdde                                       *DECORRENZA
047200170914     C     de5dde        ANDLE     LEGDSC
047300170914     C     DSCVID        ORGE      LEGDDE                                       *SCADENZA
047400170914     C     DSCVID        ANDLE     LEGDSC
047500170914     C     LEGDDE        ORGE      de5dde                                       *DECORRENZA
047600170914     C     LEGdSC        ANDLE     DSCVID
047700060907     C                   SETON                                        64
047800060907     C                   MOVEL     MSG(17)       DSMSMS
047900060907     C                   MOVEL     '1'           ERRL00
048000060907     C                   LEAVE
048100060907E   5C                   ENDIF
048200060907E   4C                   ENDIF
048300060907E   3C                   ENDIF
048400060907E   2C                   ENDDO
048500060907E   1C                   ENDIF
048600060907E  1ac                   ENDIF
048700000121     C*
048800150904     c                   if        vidnar <> *blank  and
048900150904     c                             vidnar <> vidsta
049000150904     c                   seton                                        44
049100150904     C                   end
049200000121     C*
049300150925     C*  controlla la LNP se presente su tabella LPD
049400150925     c                   if        vidlnp > 0
049500150925     C                   movel     'LPD'         TBECOD                         *TABELLA
049600150925     C                   movel(p)  vidlnp        TBEKE1                         *CHIAVE TABELLA
049700150925     C     KEYTBE        setll     tntbe01l
049800150925     c                   if        not %Equal(tntbe01l)
049900150925     C                   SETON                                        65
050000150925     C                   MOVEL     MSG(12)       DSMSMS
050100150925     C                   MOVEL     '1'           ERRL00
050200150925     C                   end
050300150925     C                   end
050400160524     C*
050500000121     C                   ENDSR
050600060907     C*------------------------------------------------------------------------*
050700060907     C* CHKDA8 - CONTROLLA LA DATA (8)
050800060907     C*------------------------------------------------------------------------*
050900060907     C     CHKDA8        BEGSR
051000060907     C*
051100060907     C     *LIKE         DEFINE    VIDDsc        WDA8
051200060907     C     *LIKE         DEFINE    VIDDsc        DA8W
051300060907     C*
051400060907     C                   Z-ADD     WDA8          G08DAT
051500060907     C                   Z-ADD     *ZEROS        G08INV
051600060907     C                   MOVEL     '0'           G08ERR
051700060907     C                   CALL      'XSRDA8'
051800060907     C                   PARM                    WLBDA8
051900060907     C                   Z-ADD     G08DAT        WDA8
052000060907     C                   Z-ADD     G08INV        DA8W
052100060907IF  1C     G08ERR        IFEQ      '1'
052200060907     C                   MOVEL     '1'           ERRL00
052300060907E   1C                   ENDIF
052400060907     C*
052500060907     C                   ENDSR
052600970526     C*------------------------------------------------------------------------*
052700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
052800970526     C*------------------------------------------------------------------------*
052900970526     C     IREVID        BEGSR
053000970526     C* CAMPI
053100170918     C                   MOVE      LEGDPC        VIDdpc
053200170915     C                   MOVEL     LEGbun        VIDbun
053300170914     c                   move      LEGdde        dataiso                        GG/MM/AAAA
053400060906     c                   move      dataiso       dataeur
053500060906     c                   move      dataeur       viddde
053600170914     C                   move      LEGnar        VIDnar
053700170914     C                   z-add     LEGksc        vidksc
053800170914     c                   move      LEGdsc        dataiso                        GG/MM/AAAA
053900060906     c                   move      dataiso       dataeur
054000060906     c                   move      dataeur       viddsc
054100170914     C                   MOVEL     LEGorma       VIDorma
054200170914     c                   move      LEGdtv        dataiso                        GG/MM/AAAA
054300060906     c                   move      dataiso       dataeur
054400060906     c                   move      dataeur       viddatv
054500170914     c                   z-add     LEGorv        vidorv
054600170914     c                   movel     LEGpru        vidpru
054700970526     C*
054800170914     c                   movel     LEGflo        dLEGflo
054900170914     c                   if        §LEGFLOLNP = *blank or
055000170914     c                             §LEGFLOLNP = *zeros
055100150924     c                   z-add     190           vidLNP
055200150924     C                   Else
055300150924     c                   move      *zeros        vidLNP
055400170914     c                   movel     §LEGFLOLNP    vidLNP
055500150924     C                   End
055600150924     C*
055700170914     c                   if        §LEGFLOGGI = *blank or
055800170914     c                             §LEGFLOGGI = *zeros
055900150924     c                   z-add     1             vidGGI
056000150924     C                   Else
056100150924     c                   move      *zeros        vidGGI
056200170914     c                   move      §LEGFLOGGI    vidGGI
056300150924     C                   End
056400160524     C*
056500160524     C*  controlla l'indirizzo MAIL presente sul DEPOT per l'invio delle
056600160524     C*  mail di apertura di Giacenza
056700160524     c     viddpc        setll     DPimd01L
056800160524     c     viddpc        reade     DPimd01L
056900160524     c                   clear                   sav_imdnrr
057000160524     c                   clear                   vidAGEml
057100160524     c                   dow       not %EoF(DPimd01L)
057200160524      **  se compreso nel periodo
057300160524    1c                   if        imddde <= datcor and imddsc >= datcor
057400160524     c                   z-add     IMD_nrr       sav_imdnrr
057500160524     c                   movel     IMDmail       vidAGEml
057600160524     c                   leave
057700160524     C                   end
057800160524      **
057900160524     c     viddpc        reade     DPimd01L
058000160524     c                   endDO
058100160524     C*
058200150924     C*
058300970526     C                   ENDSR
058400970526     C*------------------------------------------------------------------------*
058500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
058600970526     C*------------------------------------------------------------------------*
058700970526     C     AZZVID        BEGSR
058800970526     C* CAMPI
058900970526     C* DECODIFICHE
059000970526     C*
059100970526     C                   ENDSR
059200970526     C*------------------------------------------------------------------------*
059300970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
059400970526     C*------------------------------------------------------------------------*
059500170914     C     IMPLEG        BEGSR
059600970526     C*
059700170914     c                   if        vidksc <> LEGksc or dscvid <> LEGdsc
059800170914     c                             or vidorma <> LEGorma
059900170914     c                   z-add     datcor        LEGdtv
060000170914     c                   movel     wn14          LEGorv
060100170914     c                   movel     knmus         LEGpru
060200060907     c                   endif
060300060907     c
060400170914     C                   Z-ADD     VIDksc        LEGksc
060500060907     c                   if        *in91 = '1'
060600170914     C                   Z-ADD     dscvid        LEGdsc
060700170914     C                   movel     VIDorma       LEGorma
060800060907     c                   endif
060900150924     C*
061000170914     c                   move      *zeros        §LEGFLOLNP
061100170914     c                   move      vidLNP        §LEGFLOLNP
061200150924     C*
061300170914     c                   move      *zeros        §LEGFLOGGI
061400170914     c                   move      vidGGI        §LEGFLOGGI
061500150924     C*
061600170914     c                   eval      LEGflo =      dLEGflo
061700970526     C*
061800970526     C                   ENDSR
061900970526     C*------------------------------------------------------------------------*
062000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
062100970526     C*------------------------------------------------------------------------*
062200970526     C     IMPFUN        BEGSR
062300970603     C*
062400970526     C*
062500970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
062600060906    1c                   if        %subst(depop1:2:2)= '05'
062700970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
062800970526     C                   MOVEL     'N'           FUDATT
062900970526     C                   MOVEL     FUDRIG        FUD(6)
063000970526E   1C                   ENDIF
063100060907     C* SE gestione aggiunge la funzione di ricerca cod. cliente
063200060907    1c                   if        %subst(depop1:2:2)= '02'
063300060907     C                   MOVEL     FUD(7)        FUDRIG                         *AGGIORNAMENTO
063400060907     C                   MOVEL     'S'           FUDATT
063500060907     C                   MOVEL     FUDRIG        FUD(7)
063600060907E   1C                   ENDIF
063700970526     C*
063800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
063900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
064000970526     C                   EXSR      FUNGES
064100970526     C*
064200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
064300970526     C*---
064400060906     c                   setoff                                       9091
064500060906    1c                   if        %subst(depop1:2:2)= '05'
064600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
064700970526E   2C                   ENDIF
064800060906     c* se profilo edp rendo di I/O anche data scadenza e tipo ORM
064900060906     c                   if        %subst(knmus:1:3)='EDP'
065000060906     c                   seton                                        91
065100060906     c                   endif
065200970526     C*
065300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
065400060906    1c                   if        %subst(depop1:2:2)<> '06'
065500170915     C                   movel     de5dep7       KCCdep7
065600060906     C                   MOVEL     De5dde        KCCdde
065700170914     C     KEYLEG        CHAIN(N)  DPLEG11L                           99
065800970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
065900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
066000970526     C                   MOVEL     MSG(6)        DSMSMS
066100970526X   2C                   ELSE
066200970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
066300060906IF  3c                   if        %subst(depop1:2:2)= '05'
066400980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
066500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
066600980128E   3C                   ENDIF
066700980128E   2C                   ENDIF
066800970526E   1C                   ENDIF
066900970526     C*
067000970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
067100060906IF  1c                   if        %subst(depop1:2:2)= '02'
067200970526     C                   MOVEL     DEC(1)        VIDFUN
067300970526E   1C                   ENDIF
067400060906IF  1c                   if        %subst(depop1:2:2)= '05'
067500970526     C                   MOVEL     DEC(3)        VIDFUN
067600970526E   1C                   ENDIF
067700970526     C*
067800970526     C                   ENDSR
067900060907     C*--------------------------------------------------------------------------------------------*
068000060907     C* REPERISCE I DATI UTENTE
068100060907     C*--------------------------------------------------------------------------------------------*
068200060907     C     REPDATIUTE    BEGSR
068300060907     C*
068400060907     C* INIZIALIZZA VARIABILI DI WRK
068500060907     C                   CLEAR                   TIBS34DS
068600060907     C                   CLEAR                   AZUTEDS
068700060907     C                   CLEAR                   DDATIUTE
068800060907     C*
068900060907     C     *DTAARA       DEFINE    §azute        azuteds
069000060907     C     *DTAARA       DEFINE    §datiute      ddatiute
069100060907     C                   IN(E)     *DTAARA
069200060907if  1C                   IF        %Error
069300060907     c                   EVAL      I34Tla = 'L'
069400060907     C                   CALL      'TIBS34R'
069500060907     C                   PARM                    Tibs34Ds
069600060907     C                   IN        *DTAARA
069700060907e   1C                   ENDIF
069800060907     C*
069900060907     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
070000060907     C                   MOVEL(P)  RSUT          DSFIRS           20
070100060907     C*
070200060907     C                   ENDSR
070300970526     C*------------------------------------------------------------------------*
070400970526     C* *INZSR - OPERAZIONI INIZIALI
070500970526     C*------------------------------------------------------------------------*
070600970526     C     *INZSR        BEGSR
070700970526     C*---
070800970526     C* RICEVIMENTO PARAMETRI
070900970526     C*---
071000970526     C     *ENTRY        PLIST
071100970612     C                   PARM                    KPJBA
071200170914     C                   PARM                    tisie15ds
071300970526     C*
071400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
071500970526     C     XMSGPA        PLIST
071600970526     C                   PARM                    DSMSMS
071700970526     C                   PARM                    DSMSPG
071800970526     C                   PARM                    DSMSKE
071900970526     C                   PARM                    DSMSER
072000970526     C*
072100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
072200970526     C                   CALL      'XKEY'
072300970526     C                   PARM                    DSKEY
072400970526     C*---
072500970526     C* VARIABILI NON RIFERITE AL DATA BASE
072600970526     C*---
072700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
072800060906     C     *LIKE         DEFINE    De5OP0        DEPOP0                         *DI DEPOSITO
072900060906     C     *LIKE         DEFINE    De5OP1        DEPOP1
073000060906     C     *LIKE         DEFINE    De5OP0        WOP0                           *DI LAVORO
073100060906     C     *LIKE         DEFINE    De5OP1        WOP1
073200970526     C*---
073300970526     C* VARIABILI DI CONTROLLO
073400970526     C*---
073500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
073600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
073700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
073800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
073900000120     C                   MOVEL     *BLANKS       WPGM             10            *ERRORE
074000970526     C*
074100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
074200170914     C                   MOVEL     'TISIE15R'    DSMSPG
074300170914     C                   MOVEL     'TISIE15R'    MSGQUE
074400970526     C                   MOVEL     *BLANKS       MSGKEY
074500970526     C                   MOVEL     *BLANKS       DSMSMS
074600970526     C*---
074700970526     C* VARIABILI RIFERITE AL DATA BASE
074800970526     C*---
074900970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
075000970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
075100970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
075200170915     C     *LIKE         DEFINE    de5dep7       KCCdep7                        *DPCPC00F
075300170914     C     *LIKE         DEFINE    LEGdde        KCCdde
075400970526     C*---
075500970526     C* CHIAVI DI LETTURA
075600970526     C*---
075700970526     C* LETTURA TABEL00F - COMPLETA
075800970526     C     KEYTAB        KLIST
075900970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
076000970526     C                   KFLD                    KTBCOD                         *TABELLA
076100970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
076200150925     C* LETTURA TNTBE00F -
076300150925     C     KEYTBE        KLIST
076400150925     C                   KFLD                    TBECOD                         *TABELLA
076500150925     C                   KFLD                    TBEKE1                         *CHIAVE TABELLA
076600170914     C* LETTURA DPLEG11L - COMPLETA
076700170914     C     KEYLEG        KLIST
076800170915     C                   KFLD                    KCCdep7                        *VERSIONE
076900060906     C                   KFLD                    KCCdde                         *NAZIONE
077000170915     C* DPCDP11I - Totale
077100060906     C     KEYcdp        KLIST
077200170915     C                   KFLD                    cdpATB
077300170918     C                   KFLD                    osid1ver
077400170915     C                   KFLD                    LEGdep7
077500970526     C*
077600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
077700060906     C                   MOVEL     De5OP0        DEPOP0                         *LIVELLO PROCEDURA
077800060906     C                   MOVEL     De5OP1        DEPOP1                         *LIVELLO PROGRAMMA
077900060907     C* REPERIMENTO DATI UTENTE
078000060907     C                   EXSR      REPDATIUTE
078100060918     C* CALCOLA LA DATA CORRENTE
078200060918     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
078300060918     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
078400060918     c                   move      wn8           dataeur                        GG/MM/AAAA
078500060918     c                   move      dataeur       dataiso
078600060918     c                   move      dataiso       datcor            8 0
078700170918     c* recupero l'ultima versione di cappario
078800170918     C                   clear                   ISID1DRI
078900170918     C                   clear                   OSID1VER
079000170918     C                   clear                   OSID1VERD
079100170918     C                   clear                   OSID1DDE
079200170918     C                   clear                   OSID1DSC
079300170918     C                   clear                   OSID1BCID
079400170918     c                   call      'TISID1R'
079500170918     C                   PARM                    ISID1DRI
079600170918     C                   PARM                    OSID1VER
079700170918     C                   PARM                    OSID1VERD
079800170918     C                   PARM                    OSID1DDE
079900170918     C                   PARM                    OSID1DSC
080000170918     C                   PARM                    OSID1BCID
080100970526     C*
080200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
080300970526     C                   EXSR      IMPFUN
080400970526     C*
080500970526     C                   ENDSR
080600970526     C*------------------------------------------------------------------------*
080700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
080800970526     C*------------------------------------------------------------------------*
080900970526     C     FUNGES        BEGSR
081000970526     C*
081100970526     C* PULISCO CAMPI DI LAVORO
081200970526     C                   Z-ADD     0             W                 5 0
081300970526     C*
081400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
081500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
081600970526     C     1             DO        24            I
081700970526     C                   MOVEL     FUD(I)        FUDRIG
081800970526     C*
081900970526     C* PULIZIA FUNZIONI PRECEDENTI
082000970526     C                   MOVEL     *BLANKS       FUV(I)
082100970526     C*
082200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
082300970526     C* (SALTA F24)
082400970526     C     FUDATT        IFEQ      'S'
082500970526     C     FUDFUN        ANDNE     '24'
082600970526     C                   ADD       1             W
082700970526     C                   MOVEL     FUDDES        FUV(W)
082800970526     C                   END
082900970526     C                   END                                                    DO WHILE
083000970526     C*
083100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
083200970526     C* ALTRIMENTI DISATTIVO F24
083300970526     C     1             DO        24            I
083400970526     C                   MOVEL     FUD(I)        FUDRIG
083500970526     C*
083600970526     C     FUDFUN        IFEQ      '24'
083700970526     C     W             IFGT      6
083800970526     C                   MOVEL     'S'           FUDATT
083900970526     C                   ELSE
084000970526     C                   MOVEL     *BLANKS       FUDATT
084100970526     C                   END                                                    W GT 8
084200970526     C                   MOVEL     FUDRIG        FUD(I)
084300970526     C                   END
084400970526     C                   END                                                    DO WHILE
084500970526     C*
084600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
084700970526     C     W             DIV       6             I
084800970526     C                   MVR                     Z
084900970526     C*
085000970526     C     Z             IFNE      0
085100970526     C     I             ADD       1             Z
085200970526     C                   ELSE
085300970526     C                   Z-ADD     I             Z                 5 0
085400970526     C                   END
085500970526     C*
085600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
085700970526     C                   ADD       1             FUNGRU
085800970526     C     FUNGRU        IFGT      Z
085900970526     C     FUNGRU        ORLT      1
086000970526     C                   Z-ADD     1             FUNGRU
086100970526     C                   END
086200970526     C*
086300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
086400970526     C     FUNGRU        IFEQ      1
086500970526     C                   MOVEL     FUV(1)        FUN001
086600970526     C                   MOVEL     FUV(2)        FUN002
086700970526     C                   MOVEL     FUV(3)        FUN003
086800970526     C                   MOVEL     FUV(4)        FUN004
086900970526     C                   MOVEL     FUV(5)        FUN005
087000970526     C                   MOVEL     FUV(6)        FUN006
087100970526     C                   END
087200970526     C*
087300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
087400970526     C     FUNGRU        IFEQ      2
087500970526     C                   MOVEL     FUV(7)        FUN001
087600970526     C                   MOVEL     FUV(8)        FUN002
087700970526     C                   MOVEL     FUV(9)        FUN003
087800970526     C                   MOVEL     FUV(10)       FUN004
087900970526     C                   MOVEL     FUV(11)       FUN005
088000970526     C                   MOVEL     FUV(12)       FUN006
088100970526     C                   END
088200970526     C*
088300970526     C* TERZO GRUPPO DI 6 FUNZIONI
088400970526     C     FUNGRU        IFEQ      3
088500970526     C                   MOVEL     FUV(13)       FUN001
088600970526     C                   MOVEL     FUV(14)       FUN002
088700970526     C                   MOVEL     FUV(15)       FUN003
088800970526     C                   MOVEL     FUV(16)       FUN004
088900970526     C                   MOVEL     FUV(17)       FUN005
089000970526     C                   MOVEL     FUV(18)       FUN006
089100970526     C                   END
089200970526     C*
089300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
089400970526     C     FUNGRU        IFEQ      4
089500970526     C                   MOVEL     FUV(19)       FUN001
089600970526     C                   MOVEL     FUV(20)       FUN002
089700970526     C                   MOVEL     FUV(21)       FUN003
089800970526     C                   MOVEL     FUV(22)       FUN004
089900970526     C                   MOVEL     *BLANKS       FUN005
090000970526     C                   MOVEL     *BLANKS       FUN006
090100970526     C                   END
090200970526     C*
090300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
090400970526     C                   MOVEL     *BLANKS       FUN007
090500970526     C                   MOVEL     *BLANKS       FUN008
090600970526     C*
090700970526     C     1             DO        24            I
090800970526     C                   MOVEL     FUD(I)        FUDRIG
090900970526     C     FUDFUN        IFEQ      '24'
091000970526     C     FUDATT        ANDEQ     'S'
091100970526     C                   MOVEL     FUDDES        FUN008
091200970526     C                   END
091300970526     C                   END                                                    DO
091400970526     C*
091500970526     C                   ENDSR
091600970526     C*------------------------------------------------------------------------*
091700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
091800970526     C*------------------------------------------------------------------------*
091900970526     C     FUNCON        BEGSR
092000970526     C*
092100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
092200970526     C                   EXSR      FUNVAR
092300970526     C*
092400970526     C* PULISCO CAMPI DI LAVORO
092500970526     C                   Z-ADD     0             I                 5 0
092600970526     C                   Z-ADD     0             W                 5 0
092700970526     C*
092800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
092900970526     C     FUNKEY        IFNE      *BLANKS
093000970526     C     1             DO        24            I
093100970526     C                   MOVEL     FUD(I)        FUDRIG
093200970526     C*
093300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
093400970526     C     FUNKEY        IFEQ      FUDFUN
093500970526     C     FUDATT        ANDEQ     'S'
093600970526     C                   Z-ADD     I             W
093700970526     C                   END
093800970526     C                   END                                                    DO WHILE
093900970526     C*
094000970526     C* FUNZIONE NON ATTIVA (ERRORE)
094100970526     C     W             IFEQ      0
094200970526     C*
094300970526     C* EMISSIONE MESSAGGIO DI ERRORE
094400970526     C                   MOVEL     MSG(1)        DSMSMS
094500970526     C*
094600970526     C                   END                                                    W=0
094700970526     C                   END                                                    FUNKEY NE *BLANKS
094800970526     C*
094900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
095000970526     C     W             IFNE      0
095100970526     C     FUNKEY        OREQ      *BLANKS
095200970526     C     FUNKEY        IFEQ      *BLANKS
095300970526     C                   EXSR      ENTGES
095400970526     C                   END
095500970526     C     FUNKEY        IFEQ      '03'
095600970526     C                   EXSR      F03GES
095700970526     C                   END
095800060907     C     FUNKEY        IFEQ      '07'
095900060907     C                   EXSR      f07ges
096000060907     C                   END
096100970526     C     FUNKEY        IFEQ      '06'
096200970526     C                   EXSR      F06GES
096300970526     C                   END
096400970526     C     FUNKEY        IFEQ      '12'
096500970526     C                   EXSR      F12GES
096600970526     C                   END
096700970526     C     FUNKEY        IFEQ      '24'
096800970526     C                   EXSR      F24GES
096900970526     C                   END
097000970526     C                   END
097100970526     C*
097200970526     C                   ENDSR
097300970526     C*------------------------------------------------------------------------*
097400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
097500970526     C*------------------------------------------------------------------------*
097600970526     C     FUNVAR        BEGSR
097700970526     C*
097800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
097900970526     C*
098000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
098100970526     C     KEY           IFEQ      ENTER
098200970526     C                   MOVEL     *BLANKS       FUNKEY
098300970526     C                   END
098400970526     C     KEY           IFEQ      F1
098500970526     C                   MOVEL     '01'          FUNKEY
098600970526     C                   END
098700970526     C     KEY           IFEQ      F2
098800970526     C                   MOVEL     '02'          FUNKEY
098900970526     C                   END
099000970526     C     KEY           IFEQ      F3
099100970526     C                   MOVEL     '03'          FUNKEY
099200970526     C                   END
099300970526     C     KEY           IFEQ      F4
099400970526     C                   MOVEL     '04'          FUNKEY
099500970526     C                   END
099600970526     C     KEY           IFEQ      F5
099700970526     C                   MOVEL     '05'          FUNKEY
099800970526     C                   END
099900970526     C     KEY           IFEQ      F6
100000970526     C                   MOVEL     '06'          FUNKEY
100100970526     C                   END
100200970526     C     KEY           IFEQ      F7
100300970526     C                   MOVEL     '07'          FUNKEY
100400970526     C                   END
100500970526     C     KEY           IFEQ      F8
100600970526     C                   MOVEL     '08'          FUNKEY
100700970526     C                   END
100800970526     C     KEY           IFEQ      F9
100900970526     C                   MOVEL     '09'          FUNKEY
101000970526     C                   END
101100970526     C     KEY           IFEQ      F10
101200970526     C                   MOVEL     '10'          FUNKEY
101300970526     C                   END
101400970526     C     KEY           IFEQ      F11
101500970526     C                   MOVEL     '11'          FUNKEY
101600970526     C                   END
101700970526     C     KEY           IFEQ      F12
101800970526     C                   MOVEL     '12'          FUNKEY
101900970526     C                   END
102000970526     C     KEY           IFEQ      F13
102100970526     C                   MOVEL     '13'          FUNKEY
102200970526     C                   END
102300970526     C     KEY           IFEQ      F14
102400970526     C                   MOVEL     '14'          FUNKEY
102500970526     C                   END
102600970526     C     KEY           IFEQ      F15
102700970526     C                   MOVEL     '15'          FUNKEY
102800970526     C                   END
102900970526     C     KEY           IFEQ      F16
103000970526     C                   MOVEL     '16'          FUNKEY
103100970526     C                   END
103200970526     C     KEY           IFEQ      F17
103300970526     C                   MOVEL     '17'          FUNKEY
103400970526     C                   END
103500970526     C     KEY           IFEQ      F18
103600970526     C                   MOVEL     '18'          FUNKEY
103700970526     C                   END
103800970526     C     KEY           IFEQ      F19
103900970526     C                   MOVEL     '19'          FUNKEY
104000970526     C                   END
104100970526     C     KEY           IFEQ      F20
104200970526     C                   MOVEL     '20'          FUNKEY
104300970526     C                   END
104400970526     C     KEY           IFEQ      F21
104500970526     C                   MOVEL     '21'          FUNKEY
104600970526     C                   END
104700970526     C     KEY           IFEQ      F22
104800970526     C                   MOVEL     '22'          FUNKEY
104900970526     C                   END
105000970526     C     KEY           IFEQ      F23
105100970526     C                   MOVEL     '23'          FUNKEY
105200970526     C                   END
105300970526     C     KEY           IFEQ      F24
105400970526     C                   MOVEL     '24'          FUNKEY
105500970526     C                   END
105600970526     C     KEY           IFEQ      ROLLUP
105700970526     C                   MOVEL     'RU'          FUNKEY
105800970526     C                   END
105900970526     C     KEY           IFEQ      HELP
106000970526     C                   MOVEL     'HP'          FUNKEY
106100970526     C                   END
106200970526     C                   ENDSR
106300970526     C*------------------------------------------------------------------------*
106400970526     C* NOTE COSTRUZIONE SCHIERE
106500970526     C*------------------------------------------------------------------------*
106600970526     C*
106700970526     C* -MSG-MESSAGGI
106800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
106900970526     C* (NON TOCCARE MSG,1 E MSG,2)
107000970526     C*
107100970526     C* -CMD-COMANDI
107200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
107300970526     C*
107400970526     C* -OPD-OPZIONI
107500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
107600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
107700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
107800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
107900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
108000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
108100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
108200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
108300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
108400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
108500970526     C*
108600970526     C* -FUD-FUNZIONI
108700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
108800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
108900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
109100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
109300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
109400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
109500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
109600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
109700970526     O*------------------------------------------------------------------------*
109800970526** DEC - DECODIFICHE
109900970526    MODIFICA                                                      1
110000970526  ANNULLAMENTO                                                    2
110100970526VISUALIZZAZIONE                                                   3
110200970526   RIPRISTINO                                                     4
110300970526  INSERIMENTO                                                     5
110400970526** MSG - MESSAGGI
110500060906TISIE5R- Tasto funzionale non gestito                                          1
110600060906TISIE5R- Inserimento effettuato                                                2
110700060906TISIE5R- Modifica effettuata                                                   3
110800060906TISIE5R- Annullamento effettuato                                               4
110900060906TISIE5R- Ripristino effettuato                                                 5
111000060906TISIE5R- Richiesta funzione per record inesistente                             6
111100060906TISIE5R- Record momentaneamente allocato: riprovare più tardi                  7
111200060906TISIE5R- Richiesta funzione per record esistente                               8
111300060919TISIE5R- Codice inesistente su Piano dei Conti                                 9
111400060922TISIE5R- Il record non può scadere in un giorno già trascorso                 10
111500060919TISIE5R- 11                                                                   11
111600150925TISIE5R- Linea NON corretta per DPD import Bolle                              12
111700060919TISIE5R- 13                                                                   13
111800060919TISIE5R- Data errata                                                          14
111900060919TISIE5R- La data scadenza deve essere maggiore della decorrenza               15
112000060906TISIE5R-                                                                      16
112100060919TISIE5R- Il record si sovrappone ad una decorrenza/scadenza già presente      17
112200060906TISIE5R-                                                                      18
112300060906TISIE5R-                                                                      19
112400060919TISIE5R- 20                                                                   20
112500060906TISIE5R-                                                                      21
112600060906TISIE5R- 22
112700060906TISIE5R- 23
112800060906TISIE5R- 24
112900060906TISIE5R-                                                                      25
113000060919TISIE5R- 26                                                                   26
113100060906TISIE5R-                                                                      27
113200060906TISIE5R-                                                                      28
113300060906TISIE5R-                                                                      29
113400060906TISIE5R-                                                                      30
113500970526** FUD - FUNZIONI
113600970526                   -        (S/N)=N                        funz.01¬
113700970530                   -        (S/N)=N                        funz.02¬
113800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
113900970526                   -        (S/N)=N                        funz.04¬
114000970526                   -        (S/N)=N                        funz.05¬
114100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
114200060907F07=Ricerca Cliente-        (S/N)=N                        funz.07¬
114300000120                   -        (S/N)=N                        funz.08¬
114400000120                   -        (S/N)=N                        funz.09¬
114500000120                   -        (S/N)=N                        funz.10¬
114600970526                   -        (S/N)=N                        funz.11¬
114700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
114800970526                   -        (S/N)=N                        funz.13¬
114900000120                   -        (S/N)=N                        funz.14¬
115000970526                   -        (S/N)=N                        funz.15¬
115100970526                   -        (S/N)=N                        funz.16¬
115200970526                   -        (S/N)=N                        funz.17¬
115300970526                   -        (S/N)=N                        funz.18¬
115400970526                   -        (S/N)=N                        funz.19¬
115500970526                   -        (S/N)=N                        funz.20¬
115600970526                   -        (S/N)=N                        funz.21¬
115700970526                   -        (S/N)=N                        funz.22¬
115800970526                   -        (S/N)=N                        funz.23¬
115900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
