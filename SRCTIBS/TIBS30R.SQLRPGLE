000100011112     H DECEDIT('0,') DATEDIT(*YMD.)
000200120213     H DFTACTGRP(*NO)
000300161018     h bnddir('UBBNDDIR')
000400011112      *****************************************************************
000500011112      *
000600011112      * Gestione Profili utente
000700011112      *
000800011112      *****************************************************************
000900980521      *  RIEPILOGO INDICATORI
001000981008      ****************************************************************
001100011129      *
001200011120      * 20    - SflDsp
001300011120      *N21    - SflClr
001400011120      * 21    - SflDspCtl
001500011120      * 22    - SflNxtChg
001600011130      * 23    - Lettura ultimo record
001700011108      * 24    - Nessun record trovato
001800011120      * 25    - Rollup
001900011120      * 28    - Emissione messaggio di errore
002000011120      * 29    - Readc su sfl
002100130704      * 38    - In rosso se profilo ha AUT diverse da Profilo STD
002200100409      * 39    - Evidenziazione profilo scaduto nel subfile
002300011109      * 40    - Errore su opzione (sfl)
002400011203      * 41    - Errore su filiale          (prima videata e control)
002500011203      * 42    - Errore su funzione         (prima videata e control)
002600011203      * 43    - Errore su livello autorità (prima videata e control)
002700130704      * 44    - Errore su tipo modulo      (prima videata e control)
002800140710      * 51    - In blu se profilo con funzione aziendale COCOM - REA - RED
002900990317      ****************************************************************
003000011109     FAzUte01l  IF   E           K DISK
003100100416     FTnTbe01l  IF   E           K DISK
003200011112     FAzOrg01l  IF   E           K DISK
003300060512     Fkpprf01l  IF   E           K DISK
003400011203     FTibs30d   CF   E             WORKSTN  sfile(BS30S1:RecSf)
003500980521      *------------------------------------------------------------------------*
003600160822      *__________
003700160822      * Costanti
003800160822      *¯¯¯¯¯¯¯¯¯¯
003900160822      * ?set caratteri Maiuscoli/Minuscoli
004000160822     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004100160822     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
004200160822       // -?Caratteri minuscoli?
004300160822     d c_Minusc        c                   const('abcdefghijklm+
004400160822     d                                            nopqrstuvwxyz+
004500160822     d                                            àáâãäå+
004600160822     d                                            æç+
004700160822     d                                            èéêë+
004800160822     d                                            ìíîï+
004900160822     d                                            òóôõö+
005000160822     d                                            ùúûü')
005100160822
005200160822       // -?Caratteri maiuscoli?
005300160822     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
005400160822     d                                            NOPQRSTUVWXYZ+
005500160822     d                                            ÀÁÂÃÄÅ+
005600160822     d                                            ÆÇ+
005700160822     d                                            ÈÉÊË+
005800160822     d                                            ÌÍÎÏ+
005900160822     d                                            ÒÓÔÕÖ+
006000160822     d                                            ÙÚÛÜ')
006100161018
006200160822      *_________________________________
006300160822      * Schiere a tempo di compilazione
006400160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
006500160822       // -?Caratteri alfabetici?
006600160822     d sk_alfab        s              1    dim(26) ctdata perrcd(26)
006700160822
006800160822       // -?Messaggi di errore?
006900161018     D Msg             S             78    DIM(13) CTDATA PERRCD(1)
007000130703
007100160822      *_______________________________
007200160822      * Schiere a tempo di esecuzione
007300160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
007400130703       // -?Schiere per abilitazioni profili standard
007500130703     d skUTE           s             10    dim(999)
007600130703     d skABI           s            500    dim(999)
007700130703     d skAUT           s              2    dim(999)
007800130703     d skFUN           s              5    dim(999)
007900130703
008000160822      *___________
008100160822      * Variabili
008200160822      *¯¯¯¯¯¯¯¯¯¯¯
008300011109     D $Fine           S              1    INZ('N')                             *fine programma
008400011109     D TipVid          S              1    INZ('1')                             *video in gestione
008500160822     D SaveTipVid      s              1    inz                                  *video in gestione
008600011109     D $CarD1          S              1    INZ('1')                             *video in gestione
008700011112     D $CarSfl         S              1    INZ('0')                             *video in gestione
008800011220     D $Leave10        S              1    INZ('0')                             *video in gestione
008900130704     d wFIL            s                   like(V1CFil)
009000130705     d wUTE            s                   like(V1Cute)
009100011112     D W1Ute           S                   like(V1CUte)
009200011112     D W1Fil           S                   like(V1CFil)
009300011112     D W1Fun           S                   like(V1CFun)
009400011112     D W1Aut           S                   like(V1CAut)
009500110110     D W1txt           S                   like(V1Ctxt)
009600130704     d w1mopr          s                   like(V1Cmopr)
009700011120     D W2Ute           S                   like(V1CUte)
009800011120     D W2Fil           S                   like(V1CFil)
009900011120     D W2Fun           S                   like(V1CFun)
010000011120     D W2Aut           S                   like(V1CAut)
010100110110     D W2txt           S                   like(V1CTxt)
010200130704     d w2mopr          s                   like(V1Cmopr)
010300160822     d wPassword       s                   like(W03psw)  inz
010400110110     d wprftxt         s                   like(prftxt)
010500110110     d wrk2txt         s             44    varying
010600011120     d RecSf           S              4  0                                      *n° rel sf
010700011120     D WMax            S                   like($RecSf)
010800011112     D Win             S             99A
010900100409     d wDate           s              8  0 inz
011000100409     d wDate_Iso       s               d   inz  datfmt(*iso)
011100140522     d wrkUTE          s             10    inz
011200160822     d wP              s              7  0 inz
011300130703
011400130703       // -?Flags booleani
011500130703     d wEnd            s               n   inz(*off)
011600130703
011700130703       // -?Indici di schiera
011800130703     d xx              s              4  0 inz
011900130704     d yy              s              4  0 inz
012000130703
012100160822      *________________
012200160822      * Strutture dati
012300160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012400161018     d Status         sds
012500161018     d   SDSpgm          *proc
012600161018
012700160822     D kpjba         E DS
012800160822     D Tibs31ds      e ds                  inz
012900160822     D Tibs31ds2     e ds                  inz   qualified
013000160822     D Tibs32ds      e ds                  inz
013100160822     D Tibs02Ds      E DS
013200160822     D*m TntbAUTds     E DS
013300011120      *-------------
013400011120     D WLBDA8          DS                  INZ
013500011120     D  G08DAT                 1      8  0
013600011120     D  G08INV                 9     16  0
013700011120     D  G08ERR                17     17
013800011120     D  G08TGI                18     22  0
013900020820      *-----------------------------------------------------------
014000020820      * Ds di riferimento al file esterno AzUte00F
014100020820      *--------
014200020820     D AzuteDs       E DS                  ExtName(AzUte00F)
014300100114
014400100114       // - DS per flags (vedi "Modulo Profilo")
014500100114     d dUte02        e ds                  inz
014600130930     d dUte01        e ds                  inz
014700020820      *-----------------------------------------------------------
014800020820      * Ds per dati organigramma
014900020820      *--------
015000020820     D DDatiUte      E DS
015100020820      *-----------------------------------------------------------
015200020820      * Parametri per richiamo al pgm di controllo profilo utenti
015300020820      *--------
015400020820     D Tibs34Ds      E DS                  Inz
015500140522
015600140522      * Campo ORGde0 di AZORG00F
015700140522     d OG140         e ds                  inz qualified
015800140522      * Campo ORGde3 di AZORG00F
015900140522     d OG143         e ds                  inz qualified
016000140522
016100160822       //--------------------------------------------------------------
016200160822       //?Definizione procedure usate.
016300160822       //--------------------------------------------------------------
016400161018
016500161018       // -?Nome del sistema?
016600161018     d currSysNeta     s              8a   inz
016700161018       // -?Reperimento NETA sistema AS/400 corrente?
016800161018      /copy gaitrasrc/srcProtoPR,UBRTVNETA
016900161006
017000161006       // -?Gestione NOTE profilo utente?
017100161006     d TIBS43ds      e ds                  inz
017200161010     d*//iBS43opz    e                     inz('5')
017300161006     d   oBS43err    e                     inz(*off)
017400161006      *
017500161006     d TIBS43R         pr                  extpgm('TIBS43R')
017600161006     d   kpjba                             likeds(KPJBA)
017700120213
017800120213     D*--------------------------------------------------
017900120213     D* Procedure name: GetProfiloUtenteProj
018000120213     D* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
018100120213     D* Returns:        Profilo utente in Proj.
018200120213     D* Parameter:      priProfiloUtente => Profilo Utente
018300120213     D*--------------------------------------------------
018400120213     D GetProfiloUtenteProj...
018500120213     D                 PR             1A
018600120213     D  priProfiloUtente...
018700120213     D                               10A
018800130704
018900130704      //---------------------------------------------------------------
019000130704      //?Ordinamento SBFL
019100130704      //---------------------------------------------------------------
019200130704      // ----------------------------------------------------------------------
019300130704      //?Costanti per ordinamento subfile
019400130704      // ----------------------------------------------------------------------
019500130704     d MaxKey          c                   4
019600130704     d Ascendente      c                   1
019700130704     d Discendente     c                   2
019800130704     d Carattere       c                   6
019900130704     d Numerico        c                   8
020000130704     d Put             c                   1
020100130704     d EndPut          c                   2
020200130704     d Get             c                   3
020300130704      **********************************************************************
020400130704      * Campi utili:
020500130704      *     nrr        - Numero relativo di record del Subfile
020600130704      *     SizeList   - Dimensione della lista
020700130704      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
020800130704      **********************************************************************
020900130704     d NotUsed         s             16A
021000170905     d ReturnSize      s             10i 0
021100170905     d SizeList        s             10i 0
021200130704     d RrnLast         s              5I 0
021300130704     d WrkSort         s              1  0 inz(0)
021400130704      **********************************************************************
021500130704      * Data Structures
021600130704      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
021700130704      *     QLGSCB     - The sort request block for the QLGSORT API
021800130704      *     QLGSCB00   - The sort request block for the QLGSRTIO API
021900130704      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
022000130704      *     QUSEC      - Error structure for the QLGSORT API
022100130704      **********************************************************************
022200130704     d sflrcd          ds
022300130704     d  vscopz
022400130704     d  vscute
022500130704     d  vscproj
022600130704     d  vsctbaut
022700130704     d  vscdsc
022800130704     d  vscfil
022900130704     d  vsccdu
023000130704     d  vsctxt
023100130704     d  vscsif
023200130704     d  vscmopr
023300130704     d  hiddsc
023400130704     d  redute
023500140709     d  bluute
023600130709     d  hidfun
023700130704     d  selected                      1A
023800130704
023900130704      /COPY QSYSINC/QRPGLESRC,QLGSORT
024000130704     d QLGKL                         16    DIM(MaxKey)
024100130704     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
024200130704     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
024300130704     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
024400130704     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
024500130704
024600130704      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
024700130704      /COPY QSYSINC/QRPGLESRC,QUSEC
024800130704
024900130704     d Qlgsort_pr      pr                  extpgm('QLGSORT')
025000130704     d  pr_QLGSCB                          like(Qlgscb)
025100130704     d  pr_NotUsed1                        like(NotUsed)
025200130704     d  pr_NotUsed2                        like(NotUsed)
025300130704     d  pr_SizeList                        like(SizeList)
025400130704     d  pr_ReturnSize                      like(ReturnSize)
025500130704     d  pr_QUSEC                           like(Qusec)
025600130704     d                                     options(*varsize)
025700130704
025800130704     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
025900130704     d  pr_QLGSCB00                        like(QLGSCB00)
026000130704     d  pr_SflRcd                          like(SflRcd)
026100130704     d  pr_NotUsed1                        like(NotUsed)
026200130704     d  pr_SizeList                        like(SizeList)
026300130704     d  pr_NotUsed2                        like(NotUsed)
026400130704     d  pr_QUSEC                           like(Qusec)
026500130704     d                                     options(*varsize)
026600130704
026700130704     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
026800130704     d  pr_QLGSCB00                        like(QLGSCB00)
026900130704     d  pr_NotUsed1                        like(NotUsed)
027000130704     d  pr_SflRcd                          like(SflRcd)
027100130704     d  pr_SizeList                        like(SizeList)
027200130704     d  pr_NotUsed2                        like(NotUsed)
027300130704     d  pr_QUSEC                           like(Qusec)
027400130704     d                                     options(*varsize)
027500120213
027600120213      *------------------------------------------------------------------------*
027700011108      * Main
027800011108      *--------------------------------------------------------------------------------------------*
027900011108      *
028000011122     C                   EXSR      InzVar
028100130703
028200011109      * Gestione video
028300161018     C     $Fine         DOWNE     'S'
028400011109     C     TipVid        CASEQ     '1'           GesD1                           *Sfile
028500011109     C     TipVid        CASEQ     '2'           GesS1                           *Sfile
028600011107     C                   ENDCS
028700011107     C                   ENDDO
028800011112      *
028900161006     c                   clear                   TIBS43ds
029000161006     c                   eval      iBS43tla = 'C'
029100161006     c                   eval      KPJBU    = TIBS43ds
029200161006     c                   call      'TIBS43R'
029300161006     c                   parm                    KPJBA
029400161006      *
029500011107     C                   SETON                                        LR
029600011109      *--------------------------------------------------------------------------------------------*
029700011109      * GesD1  - Gestione prima videata di parzializzazione
029800011109      *--------------------------------------------------------------------------------------------*
029900011109     C     GesD1         BEGSR
030000011109      *
030100011109      * Inizializzazione videata
030200011109     C     $CarD1        ifeq      *on
030300011109     C                   EXSR      CarD1
030400011109     C                   move      *off          $CarD1
030500011109     C                   endif
030600011113      *
030700011113      * Scrivo il piede
030800011113     C                   write     Bs30P1
030900011114      *
031000011114      * scrivo il formato se ci sono errori
031100011120     C     *in28         ifeq      *on
031200011114     C                   movea     *In           Win
031300011120     C                   move      *ALL'0'       in2844           17
031400011120     C                   movea     IN2844        *in(28)
031500011114     C                   WRITE     Bs30D1
031600011114     C                   movea     Win           *In
031700011114     C                   endif
031800011109      *
031900011114      * Emetto il formato
032000011113     C                   EXFMT     Bs30D1
032100011109      *
032200011114     C     '?'           Scan      V1CFun                                 45
032300011114     C     '?'           Scan      V1CAut                                 46
032400130704     C     '?'           Scan      V1Cmopr                                49
032500130705     c                   clear                   wFIL
032600011109      *
032700011109      * Ricerca
032800011109     C                   SELECT
032900011109      *
033000011109      * F3=Fine
033100011109     C     *inKc         wheneq    *on
033200011109     C                   EXSR      F03Ges
033300011109      *
033400011114     C     *in45         wheneq    *on
033500011114     C     *in46         oreq      *on
033600130704     C     *in49         oreq      *on
033700011109     C                   EXSR      Search
033800130704
033900130704      * F8=Ordinamento x cod.utente
034000130704     c                   WHEN      *inKH
034100130704     c                   eval      wFIL = V1Cfil
034200130704     c                   EXSR      F08Ges
034300130704     c   28              seton                                        41
034400130704     c   28              eval      V1Msg = Msg(7)
034500130705     c                   clear                   wFIL
034600011109      *
034700011109      * F10= Inserimento
034800011109     C     *inKj         wheneq    *on
034900011109     C                   EXSR      F10Ges
035000011220      *
035100011203     C                   OTHER
035200011109      *
035300011109      * Controlli e decodifiche
035400011109     C                   EXSR      CtrD1
035500011109      *
035600011120     C     *IN28         ifeq      *off
035700011109      *
035800011109      * Gestisco il video di dettaglio
035900011113     C                   move      *on           $carsfl
036000011109     C                   move      '2'           TipVid
036100011120      *
036200011120      * Valorizzo ii campi del control
036300011120     C                   eval      VCCUte = V1CUte
036400011120     C                   eval      VCCFun = V1CFun
036500011120     C                   eval      VCCAut = V1CAut
036600011120     C                   z-add     V1CFil        VCCFil
036700110110     C                   eval      VCCTxt = V1CTxt
036800130704     c                   eval      VCCmopr = V1Cmopr
036900011109     C                   endif
037000011203     C                   ENDSL
037100011109      *
037200011109     C                   endsr
037300011109      *--------------------------------------------------------------------------------------------*
037400011109      * Search - Ricerca su campi video
037500011109      *--------------------------------------------------------------------------------------------*
037600011109     C     Search        BEGSR
037700011120     C                   setoff                                       28
037800011109      *
037900011109      * Funzione
038000011114     C     *in45         ifeq      *on
038100011114     C     *in47         oreq      *on
038200011114     C   45              movel     *blanks       V1CFun
038300011120     C   47              movel     *blanks       VCCFun
038400011109     C                   move      *blanks       Tibs02Ds
038500011114      *
038600011109     C                   move      'FUN'         T02Cod
038700011109     C                   move      'R'           T02Mod
038800011109     C                   movel     KnSif         T02Sif
038900011109     C                   CALL      'TIBS02R'
039000011109     C                   PARM                    kpjba
039100011109     C                   PARM                    Tibs02Ds
039200011109     C                   if        T02Err =  *blanks
039300011114     C   45              movel     T02ke1        V1CFun
039400011120     C   47              movel     T02ke1        VCCFun
039500011109     C                   endif
039600011109     C                   endif
039700011109      *
039800011109      * Autorità
039900011114     C     *in46         ifeq      *on
040000011114     C     *in48         oreq      *on
040100011114      *
040200011114     C   46              movel     *blanks       V1CAut
040300011120     C   48              movel     *blanks       VCCAut
040400011109     C                   move      *blanks       Tibs02Ds
040500011114      *
040600011112     C                   move      'LAT'         T02Cod
040700011109     C                   move      'R'           T02Mod
040800011109     C                   movel     KnSif         T02Sif
040900011109     C                   CALL      'TIBS02R'
041000011109     C                   PARM                    kpjba
041100011109     C                   PARM                    Tibs02Ds
041200011109     C                   if        T02Err =  *blanks
041300011114     C   46              movel     T02ke1        V1CAut
041400011120     C   48              movel     T02ke1        VCCAut
041500011109     C                   endif
041600011109     C                   endif
041700130704      *
041800130704      * Tipo Modulo
041900130704     C     *in49         ifeq      *on
042000130704     C     *in50         oreq      *on
042100130704      *
042200130704     C   49              movel     *blanks       V1Cmopr
042300130704     C   50              movel     *blanks       VCCmopr
042400130704     C                   move      *blanks       Tibs02Ds
042500130704      *
042600130704     C                   move      'MDP'         T02Cod
042700130704     C                   move      'R'           T02Mod
042800130704     C                   movel     KnSif         T02Sif
042900130704     C                   CALL      'TIBS02R'
043000130704     C                   PARM                    kpjba
043100130704     C                   PARM                    Tibs02Ds
043200130704     C                   if        T02Err =  *blanks
043300130704     C   49              movel     T02ke1        V1Cmopr
043400130704     C   50              movel     T02ke1        VCCmopr
043500130704     C                   endif
043600130704     C                   endif
043700011114      *
043800011114     C                   setoff                                       4546
043900011114     C                   setoff                                       4748
044000130704     C                   setoff                                       4950
044100011109     C                   ENDSR
044200011109      *--------------------------------------------------------------------------------------------*
044300011109      * F03Ges - Tasto funzionale F03 -> Fine programma
044400011109      *--------------------------------------------------------------------------------------------*
044500011109     C     F03Ges        BEGSR
044600011109      *
044700011109     C                   Movel     'S'           $Fine                          *fine pgm
044800011109      *
044900011109     C                   ENDSR
045000130704      /free
045100130704       //--------------------------------------------------------------
045200130704       //  F08Ges - Tasto funzionale F08 -> Ordinamento x Cod.Utente
045300130704       //--------------------------------------------------------------
045400130704       BEGSR  F08ges;
045500130704
045600130704       //?Serve la filiale
045700130704         IF  wFIL = 0;
045800130704           *in28 = *on;
045900130704           leavesr;
046000130704         ENDIF;
046100130704
046200130704         rrnlast = wmax;
046300130704
046400130704       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
046500130704       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
046600130704       // il campo è usato per selezionare i records dando un ordine a quelli
046700130704       // selezionati davanti ai non selezionati.
046800130704         clear QLGSCB;
046900130704         clear QLGSCB00;
047000130704
047100130704       // 2 campi chiave x Ordinamento cod.Utente - codice utente + filiale
047200130704         QLGNBRK = 2;
047300130704
047400130704       // imposto la posizione del cod.utente sul subfile e la lunghezza
047500130704       // l'ordinamento è su un campo numerico e deve essere
047600130704       // ascending
047700130704         QLGSP = 1 + %size(VSCopz)+ %size(VSCute) + %size(VSCproj) +
047800130704                     %size(VSCtbaut) + %size(VSCdsc) + %size(VSCfil);
047900130704         QLGSS = %SIZE(VSCcdu);
048000130704         QLGDT = Numerico;
048100130704         QLGSO = Ascendente;
048200130704         QLGKL(1) = QLGSKL;
048300130704
048400130704       // imposto la posizione della filiale  sul subfile e la lunghezza
048500130704       // l'ordinamento è su un campo numerico e deve essere
048600130704       // ascending
048700130704         QLGSP = 1 + %size(VSCopz)+ %size(VSCute) + %size(VSCproj) +
048800130704                     %size(VSCtbaut) + %size(VSCdsc);
048900130704         QLGSS = %SIZE(VSCfil);
049000130704         QLGDT = Numerico;
049100130704         QLGSO = Ascendente;
049200130704         QLGKL(2) = QLGSKL;
049300130704
049400130704       // Load other sort parameters.
049500130704         QLGLB = 80 + 16 * MaxKey;
049600130704         QLGRL = %SIZE(sflrcd) - 1;
049700130704         QLGRT = 8;
049800130704         QLGOKL = 80;
049900130704         QLGLKE = 16;
050000130704         QLGLSS = 290;
050100130704
050200130704       // Initialize Sort I/O API fields.
050300130704         QLGRL00 = QLGRL;
050400130704         QLGRC00 = 1;
050500130704         clear QUSEI;
050600130704         QUSBPRV = %SIZE(QUSEC);
050700130704
050800130704       // First step - Initialize the sort routine.
050900130704         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
051000130704
051100130704       // Next step - Write records to I/O routine.
051200130704         QLGRT00 = Put;
051300130704         for yy = 1 to rrnlast;
051400130704          chain yy BS30S1;
051500130704
051600130704       // solo le righe con Selected = 'Y' sono riordinate,
051700130704       // quindi per fare un ordinamento di tutte le righe
051800130704       // metto 'Y' sempre.
051900130704          selected  = 'Y';
052000130704          clear QUSEI;
052100130704          QUSBPRV = %SIZE(QUSEC);
052200130704          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
052300130704         endfor;
052400130704
052500130704       // Next step - Signal end of input, clear subfile for reload.
052600130704         QLGRT00 = EndPut;
052700130704         clear QUSEI;
052800130704         QUSBPRV = %SIZE(QUSEC);
052900130704         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
053000130704       // pulizia SFL
053100130704         clear recsf;
053200130704         *in20 = *off;
053300130704         *in21 = *off;
053400130704         write BS30C1;
053500130704         *in20 = *on;
053600130704         *in21 = *on;
053700130704
053800130704       // Final step - Write the records back to the subfile.
053900130704         QLGRT00 = Get;
054000130704         for yy = 1 to rrnlast;
054100130704          clear QUSEI;
054200130704          QUSBPRV = %SIZE(QUSEC);
054300130704          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
054400130704          recsf = yy;
054500130704          *in38 = (redute = '1');
054600140710          *in51 = (bluute = '1');
054700130704          *in39 = (HIDdsc < wDate);
054800130704          write BS30S1;
054900130704         endfor;
055000130704
055100130704         // Visualizzazione del SFL (se ci sono dati)
055200130704         *in20 = (recsf > *zeros);
055300130704
055400130704         // Posizionamento cursore al 1° rcd della pagina
055500130704         $recsf = 1;
055600130704
055700130704       ENDSR;
055800130704
055900130704      /end-free
056000130704
056100011109      *--------------------------------------------------------------------------------------------*
056200011109      * F10Ges - Tasto funzionale F10 -> Inserimento
056300011109      *--------------------------------------------------------------------------------------------*
056400011109     C     F10Ges        BEGSR
056500011210      *
056600011203     C                   clear                   Tibs31Ds
056700011203     C                   move      'I'           I31Opz
056800011220     C                   move      *off          $leave10          1
056900011109      *
057000011109      * Lo richiamo nella modalità 'nuovo record'
057100011109     C                   CALL      'TIBS31R'
057200011109     C                   PARM                    kpjba
057300011203     C                   PARM                    Tibs31Ds
057400011120      *
057500011120      *
057600011127      * Se provengo dalla prima videata ricarico la prima videata.
057700011127      * Se provengo da Sfl distinguo i casi con cui sono uscita
057800011203      * da tibs31r : se ho F03 =*on riemetto il sfl tal quale,
057900011127      *                             pulendo solo l'ultima opzione scelta
058000011127      *              se ho F06 =*on ricarico il sfl
058100011127      *
058200011203      * Se provengo dalla prima videata, o la ricarico, o se ho F03
058300011203      * attivo esco definitivamente
058400011210 2a  C     TipVid        ifeq      '1'
058500011210 3a  C     O31Tfu        ifeq      'F03'
058600011203     C                   eval      $Fine = 'S'
058700011203     C                   else
058800011109     C                   eval      $CarD1 ='1'
058900011210 3-  C                   endif
059000011210 2-  C                   endif
059100011120      *
059200011210 2a  C     TipVid        ifeq      '2'
059300011127      *
059400011210 3a  C     O31Tfu        ifeq      'F03'
059500011128      *
059600011128      * Segnalo che devo poi uscire dal loop delle readc su sfl
059700011128      * per riemetterlo tal quale e non richiamare il Tibs31R
059800011220     C                   eval      $leave10 = *on
059900011220     C                   eval      $Carsfl  ='0'
060000011210 3-  C                   endif
060100011127      *
060200011210 3a  C     O31Tfu        ifeq      'F06'
060300011120     C                   eval      $Carsfl='1'
060400011210 3-  C                   endif
060500011210 2-  C                   endif
060600011210      *
060700011109     C                   ENDSR
060800011109      *--------------------------------------------------------------------------------------------*
060900011109      * CarD1  - Carica primo video
061000011109      *--------------------------------------------------------------------------------------------*
061100011109     C     CarD1         BEGSR
061200011109      *
061300011120     c                   setoff                                       28
061400011112     c                   setoff                                       414243
061500130704     c                   setoff                                       44
061600011112     C                   clear                   V1CUte
061700011112     C                   clear                   V1CFil
061800011112     C                   clear                   V1CFun
061900011112     C                   clear                   V1CAut
062000110110     C                   clear                   V1CTxt
062100130704     C                   clear                   V1Cmopr
062200011109      *
062300011109     C                   ENDSR
062400011109      ************************************************************
062500011109      * Controlla e decodifica la prima videata
062600011109      ************************************************************
062700011109     C     CtrD1         BEGSR
062800011109      *
062900011120     C                   setoff                                       28
063000011109     C                   setoff                                       414243
063100130704     C                   setoff                                       44
063200011109     C                   clear                   V1Msg
063300011109      *
063400011112      * Controllo Filiale
063500011109     C     V1CFil        ifne      *zeros
063600011112     c                   move      V1CFil        kFil              3 0
063700011203     c     kFil          chain     AzOrg01l                           30
063800011120     C   30              seton                                        2841
063900011120     C   30              eval      V1Msg = Msg(1)
064000011112     C                   endif
064100011109      *
064200011109      * Controllo Funzione
064300011113     C     V1Cfun        ifne      *blanks
064400011109     C                   clear                   tibs02ds
064500011109     C                   movel     'C'           t02mod
064600011109     C                   movel     *blanks       t02sif
064700011109     C                   movel     'FUN'         t02cod
064800011109     C                   movel     V1CFun        t02ke1
064900011109     C                   call      'TIBS02R'
065000011109     C                   parm                    KPJBA
065100011109     C                   parm                    TIBS02DS
065200011109     C                   if        t02err <> *blanks
065300011120     C                   seton                                        2842
065400011112     C                   eval      V1Msg = Msg(2)
065500011109     C                   endif
065600011113     C                   endif
065700011109      *
065800011112      * Controllo Livello autorità
065900011113     C     V1CAut        ifne      *blanks
066000011109     C                   clear                   tibs02ds
066100011109     C                   movel     'C'           t02mod
066200011109     C                   movel     *blanks       t02sif
066300011109     C                   movel     'LAT'         t02cod
066400011109     C                   movel     V1CAut        t02ke1
066500011109     C                   call      'TIBS02R'
066600011109     C                   parm                    KPJBA
066700011109     C                   parm                    TIBS02DS
066800011109     C                   if        t02err <> *blanks
066900011120     C                   seton                                        2843
067000011109     C                   eval      V1Msg = Msg(3)
067100011109     C                   endif
067200011113     C                   endif
067300130704      *
067400130704      * Controllo Tipo Modulo
067500130704     C     V1Cmopr       ifne      *blanks
067600130704     C                   clear                   tibs02ds
067700130704     C                   movel     'C'           t02mod
067800130704     C                   movel     *blanks       t02sif
067900130704     C                   movel     'MDP'         t02cod
068000130704     C                   movel     V1Cmopr       t02ke1
068100130704     C                   call      'TIBS02R'
068200130704     C                   parm                    KPJBA
068300130704     C                   parm                    TIBS02DS
068400130704     C                   if        t02err <> *blanks
068500130704     C                   seton                                        2844
068600130704     C                   eval      V1Msg = Msg(6)
068700130704     C                   endif
068800130704     C                   endif
068900011109      *
069000011109     C                   ENDSR
069100011109      *--------------------------------------------------------------------------------------------*
069200011109      * GesS1  - Gestione video subfile
069300011109      *--------------------------------------------------------------------------------------------*
069400011109     C     GesS1         BegSr
069500011120     C                   setoff                                       24
069600011107      *
069700011109      * Inizializzo la videata
069800011210 1a  C     $CarSfl       ifeq      *on
069900011109     C                   exsr      CarSfl
070000011210 1-  C                   endif
070100011107      *
070200011109      * Scrivo il piede
070300011113     C                   write     Bs30P1
070400011109      * Se non ci sono dati da caricare e non ho richesto il posizionamento
070500011210 1a  C     wmax          ifeq      0
070600011112     c                   setoff                                       20
070700011120     c                   seton                                        2124
070800011113     C                   write     Bs30D2                                       NO RCD
070900011210 1-  C                   endif
071000011109      *
071100011109      * Emetto il control
071200011113     C                   exfmt     Bs30C1
071300011114      *
071400011120     C     '?'           Scan      VCCFun                                 47
071500011120     C     '?'           Scan      VCCAut                                 48
071600130704     C     '?'           Scan      VCCmopr                                50
071700011109      *
071800011210 1a  C                   SELECT
071900011109      *
072000011109      * F03=Fine
072100011109     C     *inkc         Wheneq    *on
072200011109     C                   EXSR      F03Ges
072300011114      *
072400011114     C     *in47         wheneq    *on
072500011114     C     *in48         oreq      *on
072600130704     C     *in50         oreq      *on
072700011114     C                   EXSR      Search
072800130704
072900130704      * F8=Ordinamento x cod.utente
073000130704     c                   WHEN      *inKH
073100130704     c                   eval      wFIL = VCCfil
073200130704     c                   EXSR      F08Ges
073300130704     c   28              seton                                        41
073400130704     c   28              eval      C1Msg = Msg(7)
073500011210      *
073600011210      * F10=Immissione
073700011210     C     *inkj         wheneq    *on
073800011210     C                   EXSR      F10Ges
073900011220      *
074000011220      * Eseguo il controllo opzioni solo se non sono
074100020108      * uscita con F03 da F10Ges e se ho dei record caricati
074200011220     C     $leave10      ifeq      *off
074300020108     C     WMax          andgt     0
074400011220     C                   EXSR      CtrS1
074500011220     C                   endif
074600011210      *
074700011210 1e  C                   OTHER
074800011112      *
074900011113      * Controllo dati e posizionamento
075000011109     C                   EXSR      CtrC1
075100011109      *
075200011120     C     *in28         ifeq      *off
075300020108     C     WMax          andgt     0
075400011220      *
075500020108      * Controllo le opzioni solo se non è stato richiesto il posizionamento
075600011220     C     $Pos          ifne      *on
075700011112     C                   EXSR      CtrS1
075800011220     C                   else
075900011220     C                   move      *off          $Pos
076000011220     C                   endif
076100011220     C                   endif
076200011109      *
076300011210 1-  C                   ENDSL
076400011109      *
076500011109     C                   ENDSR
076600011109      *--------------------------------------------------------------------------------------------*
076700011109      * Carsfl - Caricamento sfile
076800011109      *--------------------------------------------------------------------------------------------*
076900011109     C     CarSfl        BEGSR
077000011109      *
077100011126     C                   setoff                                           30
077200011109     C                   z-add     *zeros        RecSf                          *n° record sfile
077300011112     C                   movel     '2'           TipVid                         *gestione 1° video
077400011109      * Pulizia sfile
077500011112     C                   setoff                                       2021      *non visualizza sfl
077600011113     C                   write     Bs30C1
077700011112     C                   seton                                        2021
077800130704      * Prima di caricare il subfile devo caricare la SK dei profili standard
077900130704     c                   exsr      ABISTD
078000011109      *
078100011109      * Caricamento sfile -tutto-
078200011109     C                   EXSR      ScrSfl                                       *scrive sfile
078300011203      *
078400011203      * Posizionamento cursore su sfl
078500011203     C                   SELECT
078600011203      *
078700011203      * Se ho richesto rollup ma sono a fine lettura sfl,
078800011203      * continuo a posizionarmi sull'ultimo record del sfl
078900011203     C     *in25         wheneq    *on
079000011203     C     *in23         andeq     *on
079100011203     C                   z-add     wmax          $RecSf                         *posiziona in fondo
079200011203      *
079300011203      * Se ho riempito almeno una riga di sfl, e se non provengo
079400011203      * da F06 del tibs31R, mi riposiziono sul primo record.
079500011203     C     $RecSf        wheneq    0
079600011203     C     RecSf         orne      0
079700011203     C     O31Tfu        andne     'F06'
079800011203     C                   z-add     1             $RecSf                         *posiziona in fondo
079900120402      *
080000120402      * Se ho riempito almeno una riga di sfl, e se provengo
080100120402      * da F06 del tibs31R, mi riposiziono sul record in cui
080200120402      * ero  o  sull'ultimo della pagina
080300120402     C     RecSf         whenne    0
080400120402     C     O31Tfu        andeq     'F06'
080500120402     C     $RecSf        andgt     wMax
080600120402     C                   z-add     wMax          $RecSf                         *posiziona in fondo
080700120402      *
080800011203     C                   ENDSL
080900011109      *
081000011203     C                   move      '0'           $carsfl
081100011220     C                   movel     *blanks       O31Tfu
081200011203      *
081300011107     C                   ENDSR
081400011109      *--------------------------------------------------------------------------------------------*
081500011109      * ScrSfl - Scrive il sfile -tutto-
081600011109      *--------------------------------------------------------------------------------------------*
081700011109     C     ScrSfl        BEGSR
081800100409      *
081900100409      * Reperimento data odierna
082000100409     c                   eval      wDate_Iso = %date()
082100100409     c                   eval      wDate     = %dec(wDate_Iso)
082200011109      *
082300011112      * Imposta dati
082400011107     C                   z-add     0             wmax
082500011112     C                   move      *blanks       $Mod              1
082600011112     C                   move      '1'           $OkSel            1
082700011112     C                   movel     *all'9'       All9             10
082800011112      *
082900011120      * Se i campi di work sono valorizzati utilizzo questi per i confronti
083000011120      * Sposto quindi i campi da utilizzare per le selezioni in altri campi.
083100011120      * Se il profilo non è valorizzato e la filiale è diversa da zeros
083200011120      * allora imposto $Mod='2', altrimenti imposto $Mod='1'.
083300100409      * Sia per $Mod='1' che per $Mod='2' utilizzo AzUte01l.
083400011120      * Eseguo il check di validità per autorità e finzione, se $mod='1'
083500011120      * anche per filiale.
083600011203 1a  C     W1Ute         ifne      *blanks
083700011120     C                   eval      W2Ute = W1Ute
083800011203 1e  C                   else
083900011120     C                   eval      W2Ute = VCCUte
084000011203 1-  C                   endif
084100011120      *
084200011203 1a  C     W1Fun         ifne      *blanks
084300011120     C                   eval      W2Fun = W1Fun
084400011203 1e  C                   else
084500011120     C                   eval      W2Fun = VCCFun
084600011203 1-  C                   endif
084700011120      *
084800011203 1a  C     W1Aut         ifne      *blanks
084900011120     C                   eval      W2Aut = W1Aut
085000011203 1e  C                   else
085100011120     C                   eval      W2Aut = VCCAut
085200011203 1-  C                   endif
085300110105      *
085400110105 1a  C     W1txt         ifne      *blanks
085500110105     C                   eval      W2txt = W1txt
085600110105 1e  C                   else
085700110105     C                   eval      W2txt = VCCtxt
085800110105 1-  C                   endif
085900011120      *
086000011203 1a  C     W1Fil         ifne      *zeros
086100011120     C                   move      W1Fil         W2Fil
086200011203 1e  C                   else
086300011120     C                   move      VCCFil        W2Fil
086400011203 1-  C                   endif
086500130704      *
086600130704     c                   IF        w1mopr <> *blanks
086700130704     c                   eval      w2mopr = w1mopr
086800130704     c                   ELSE
086900130704     c                   eval      w2mopr = VCCmopr
087000130704     c                   ENDIF
087100011120      *
087200011203 1a  C     W2Ute         ifeq      *blanks
087300011120     C     W2Fil         andne     *zeros
087400011120     C                   eval      $Mod = '2'
087500011203 1e  C                   else
087600011120     C                   eval      $Mod = '1'
087700011203 1-  C                   endif
087800011112      *
087900011112      * Creo limite massimo delle parzializzazioni
088000011120     C     W2Ute         Cat       ALL9:0        WUte999          10
088100011120      *
088200011203 1a  C     $Mod          ifeq      '1'
088300011120     C     W2Ute         setll     AzUte01l
088400011120     C                   read      AzUte01l                               30
088500011112      *
088600011203 1e  C                   else
088700100409     C     *loval        setll     AzUte01l
088800100409     C                   read      AzUte01l                               30
088900011203 1-  C                   endif
089000011112      *
089100011112      * Ciclo di lettura sulla tabella da caricare
089200011203 1a  C                   dow       not *in30
089300011120     C                   move      '1'           $OkSel            1
089400130704     c                   eval      dUte02 = UTEfa1
089500130930     c                   eval      dUte01 = UTEfaf
089600011112      *
089700011112      * Controllo, se sono in modalità 1, il limite max del cod.profilo
089800011203 2a  C     $Mod          ifeq      '1'
089900011120     C     UteUte        andlt     W2Ute
090000011120     C     UteUte        andgt     WUte999
090100011112     C                   move      '0'           $OkSel
090200011203 2-  C                   endif
090300011112      *
090400011112      * Controllo Funzione e autorizzazione
090500011203 2a  C     W2Fun         ifne      *blanks
090600011120     C     W2Fun         andne     UteFun
090700011120     C     W2Aut         orne      *blanks
090800011120     C     W2Aut         andne     UteAut
090900100409     C     W2Fil         orne      *zeros
091000011120     C     W2Fil         andne     UteFil
091100130704     C     w2mopr        orne      *blanks
091200130704     C     w2mopr        andne     §UT2mopr
091300011120     C                   move      '0'           $OkSel
091400011203 2-  C                   endif
091500110105      * Controllo descrizione (testo)
091600110105 2a  C     W2txt         ifne      *blanks
091700110105     c     uteute        chain     kpprf01l
091800110105     c                   if        not %found(kpprf01l)
091900110105     C                   move      '0'           $OkSel
092000110105     c                   else
092100110110     c     lo:up         xlate     prftxt        wprftxt
092200110110     C                   EVAL      %len(wrk2txt) = %len(%trim(w2txt))
092300110110     c                   eval      wrk2txt = %trim(w2txt)
092400110110     c     wrk2txt       scan      wprftxt
092500110110     c                   if        not %found
092600110110     C                   move      '0'           $OkSel
092700110110     c                   endif
092800110110     c
092900110105     c                   endif
093000110105     c                   endif
093100011112      *
093200011203 2a  C     $OkSel        ifeq      '1'
093300011107      *
093400011109      * Riempimento campi subfile
093500011112     C                   exsr      RieS1
093600011112      *
093700011109      * Scrivo sfile
093800011109     C                   add       1             RecSf                          *incrementa n°rec
093900130704     c                   IF        *in38
094000130704     c                   eval      redute = '1'
094100130704     c                   ENDIF
094200140710     c                   IF        *in51
094300140709     c                   eval      bluute = '1'
094400140709     c                   ENDIF
094500011107      *
094600011203     C                   write     Bs30S1                                       *scrive sfile
094700011112     C                   z-add     RecSf         wmax                           *n° rel sf x stampa
094800011203 2-  C                   endif
094900011107      *
095000011112      * Continuo a leggere a seconda della modalità
095100011203 2a  C     $Mod          ifeq      '1'
095200011120     C                   read      AzUte01l                               30
095300011203 2-  C                   endif
095400011203 2a  C     $Mod          ifeq      '2'
095500100409     C                   read      AzUte01l                               30
095600011203 2-  C                   endif
095700011203 1-  C                   ENDDO
095800011112      *
095900011211     C                   movel     VCCUte        w1Ute
096000011211     C                   move      VCCFil        w1Fil
096100011211     C                   move      VCCFun        w1Fun
096200011211    >C                   move      VCCAut        w1Aut
096300110110    >C                   move      VCCTxt        w1txt
096400130704     c                   eval      w1mopr = VCCmopr
096500011109      *
096600011107     C                   ENDSR
096700011112      *--------------------------------------------------------------------------------------------*
096800040729      * Richiamo pgm. TIBS31R nelle diverse modalità
096900011112      *--------------------------------------------------------------------------------------------*
097000040729     C     CallTIBS31    BEGSR
097100011107      *
097200011112      * valorizzo profilo e opzione
097300011203     C                   clear                   Tibs31Ds
097400130705     C                   eval      I31Ute = wUTE
097500011203     C                   eval      I31Opz = VSCOpz
097600130705     c                   eval      I31fil = %editc(wFIL:'X')
097700140710     c                   IF        HIDfun = 'COCOM' or
097800140710     c                             HIDfun = 'REA' or
097900140710     c                             HIDfun = 'RED'
098000130717     c                   eval      I31fun = HIDfun
098100130717     c                   ENDIF
098200011107      *
098300011107      * Richiamo pgm di gestione
098400160822     c                   If        TipVid <> '3'
098500011109     C                   call      'TIBS31R'
098600011107     C                   PARM                    kpjba
098700011203     C                   PARM                    Tibs31Ds
098800160822     c                   Else
098900160822     c                   call      'TIBS31R'
099000160822     c                   parm                    KPJBA
099100160822     c                   parm                    TIBS31ds
099200160822     c                   parm                    TIBS31ds2
099300160822     c                   EndIf
099400011107      *
099500011127      * Distinguo i casi con cui sono uscito da tibs31r:
099600011127      *              se ho F03 =*on riemetto il sfl tal quale,
099700011127      *                             pulendo solo l'ultima opzione scelta
099800011127      *              se ho F06 =*on ricarico il sfl
099900011127      *
100000011210 1A  C     O31Err        IFEQ      *blanks
100100011127      *
100200011210 2a  C     O31Tfu        ifeq      'F03'
100300011128      *
100400011128      * Segnalo che devo poi uscire dal loop delle readc su sfl
100500011128      * per riemetterlo tal quale e non richiamare il Tibs31R
100600011128      * se presenti eventuali altre opzioni.
100700011127     C                   eval      $leave  = *on
100800011127     C                   eval      $Carsfl ='0'
100900011210 2-  C                   endif
101000011127      *
101100011210 2a  C     O31Tfu        ifeq      'F06'
101200011127     C                   eval      $Carsfl='1'
101300011210 2-  C                   endif
101400011210      *
101500011210      * C'è un errore nel richiamo di tibs31r
101600011210      * emetto il meggaggio.
101700011210 1E  C                   ELSE
101800011210      *
101900011210     C                   clear                   C1Msg
102000011210     C                   seton                                        28
102100011210     C                   eval      C1Msg = O31Msg
102200011210 1-  C                   endif
102300011127      *
102400011107     C                   ENDSR
102500040729      *-----------------------------------------------------------
102600040729      * Richiamo pgm. TIBS32R
102700040729      *-----------------------------------------------------------
102800040729     c     CallTIBS32    BEGSR
102900040729      *
103000040729     c                   clear                   TIBS32ds
103100040729     c                   select
103200080912     c                   when      VSCopz = '1'
103300080912     c                   eval      I32opz = '7'
103400081030     c                   when      VSCopz = '4'
103500081030     c                   eval      I32opz = '8'
103600040729     c                   when      VSCopz = '7'
103700040729     c                   eval      I32opz = '5'
103800040729     c                   when      VSCopz = '8'
103900040729     c                   eval      I32opz = '2'
104000040729     c                   endsl
104100040729     c                   eval      I32ute = VSCute
104200040729     c                   movel(p)  TIBS32ds      KPJBU
104300040729      *
104400040729     c                   call      'TIBS32R'
104500040729     c                   parm                    KPJBA
104600040729      *
104700040729     c                   movel     KPJBU         TIBS32ds
104800040729     c                   if        O32err = 'E'
104900040729     c                   clear                   $Fine
105000040729     c                   seton                                        28  40
105100040729     c                   movel     O32msg        C1msg
105200040729     c                   endif
105300040729      *
105400040729     c                   ENDSR
105500161006      *-----------------------------------------------------------
105600161006      * Richiamo pgm. TIBS43R
105700161006      *-----------------------------------------------------------
105800161006     c     sr_CallTIBS43 BEGSR
105900161006      *
106000161006     c                   reset                   TIBS43ds
106100161006     c                   eval      iBS43ute = VSCute
106200161006     c                   movel(p)  TIBS43ds      KPJBU
106300161006      *
106400161006     c                   call      'TIBS43R'
106500161006     c                   parm                    KPJBA
106600161006      *
106700161006     c                   movel     KPJBU         TIBS43ds
106800161006     c                   select
106900161006     c                   when      oBS43err = *on
107000161006     c                   clear                   $Fine
107100161006     c                   seton                                        28  40
107200161006     c                   movel     oBS43msg      C1msg
107300161006     c                   when      oBS43fxx = '03'
107400161006     C                   move      *on           $Leave
107500161006     c                   endsl
107600161006      *
107700161006     c                   ENDSR
107800100407      *-----------------------------------------------------------
107900100407      * Richiamo pgm. TNTBAUTR
108000100407      *-----------------------------------------------------------
108100130930     c*m   CallTBAUTR    BEGSR
108200100407      *
108300130930     c*m                 clear                   TNTBautDS
108400130930     c*m                 eval      autSELEZ = 'R'
108500130930     c*m                 move(p)   vscFIL        autFILIA
108600130930     c*m                 eval      autPROF  = vscUTE
108700130930     c*m                 movel(p)  TNTBautDS     KPJBU
108800100407      *
108900130930     c*m                 call      'TNTBAUTR'
109000130930     c*m                 parm                    KPJBA
109100130930     c*m                 movel     KPJBU         TNTBautDS
109200130930      *m
109300130930     c*m                 ENDSR
109400011109      *--------------------------------------------------------------------------------------------*
109500011109      * CtrS1  - Controlla campi sfile
109600011109      *--------------------------------------------------------------------------------------------*
109700011109     C     Ctrs1         BEGSR
109800011109      *
109900011210     C                   setoff                                       222990
110000130709     c                   clear                   wFIL
110100011107      *
110200011109      * Lettura dei record "toccati" del sfile
110300011109     C                   z-add     1             RecSf
110400011203     C                   readc     Bs30s1                                 29
110500011220     C                   dow       not *in29
110600011107      *
110700011109      * Gestione opzioni
110800011107      *
110900011107      * Opzioni
111000040729     C                   if        VSCopz <> ' '
111100011107      *
111200040729     c                   select
111300130704     c                   when      knmus <> 'EDPAC' and
111400130704     c                             knmus <> 'EDPND' and
111500130704     c                             knmus <> 'EDPMB' and
111600130705     c                             knmus <> 'EDPGA' and
111700130705     c                             knmus <> 'EDPLB' and
111800130704     c                             VSCmopr = 'S' and
111900130704     c                             VSCopz <> '3' and
112000130704     c                             VSCopz <> '5' and
112100130704     c                             VSCopz <> '7'
112200130704     c                   seton                                        2840
112300130704     C                   eval      C1Msg = Msg(4)
112400130723      * Copia da profilo no STD no 046 (tolgo questo mess fino a quando non ci sono profili STD)
112500130808     c******             when      knmus <> 'EDPAC' and
112600130808     c******                       knmus <> 'EDPND' and
112700130808     c******                       knmus <> 'EDPMB' and
112800130808     c******                       knmus <> 'EDPGA' and
112900130808     c******                       knmus <> 'EDPLB' and
113000130808     c******                       VSCmopr <> 'S' and
113100130808     c******                       VSCfil <> 046 and
113200130808     c******                       VSCopz = '3'
113300130723      * (tolgo questa messaggio fino a quando non ci sono profili STD)
113400130723     c******             seton                                        2840
113500130723     C******             eval      C1Msg = Msg(4)
113600161018      * Copia da profilo no STD - su SETRAS, non su AS888
113700161018     c                   when      %subst( currSysNetA : 1 : 6 )
113800161018     c                                     =  'SETRAS' and
113900161018     c                             kNmUs   <> 'EDPAC'  and
114000161018     c                             kNmUs   <> 'EDPND'  and
114100161018     c                             kNmUs   <> 'EDPMB'  and
114200161018     c                             kNmUs   <> 'EDPGA'  and
114300161018     c                             kNmUs   <> 'EDPLB'  and
114400161018     c                             VSCmopr <> 'S'      and
114500161018     c******                       VSCfil  <> 046      and
114600161018     c                             VSCopz  =  '3'
114700161018     c                   exsr      sr_GesW04
114800130705      * Copia da profilo STD
114900130705     c                   when      VSCopz = '3' and VSCmopr = 'S'
115000160819     c*//                exsr      gesw03
115100160819     c                   exsr      sr_GesWO3
115200040729      * Gestione richiamo TIBS31R
115300100114     C                   when      VSCopz = '2' or VSCopz = '3' or
115400100114     C                             VSCopz = '5' or VSCopz = '9'
115500130705     c                   eval      wUTE = VSCute
115600040729     c                   exsr      CallTIBS31
115700170905     *** *   Controlla se è stata annullata la tabella quindi pulisce il flag
115800170905     *** *      di Profilo presente sulla tabella "AUT"
115900170905     ***c                   clear                   vscTBaut
116000170905     ***c                   if        §UTEAUTOR <> 'NO' and §UTEAUTOR <> *blank
116100170905     ***c                   move      'S'           vscTBaut
116200170905     ***c                   end
116300040729      * Gestione richiamo TIBS32R
116400080912     C                   when      VSCopz = '7' or VSCopz = '8' or
116500081030     C                             VSCopz = '1' or VSCopz = '4'
116600040729     c                   exsr      CallTIBS32
116700100407      * Gestione richiamo TNTBAUTR
116800130930     C*m                 when      VSCopz = 'A'
116900130930     c*m                 exsr      CallTBAUTR
117000161006      * Gestione richiamo TIBS43R x Visualizzazione Note
117100161006     c                   when      VSCopz = 'N'
117200161006     c                   exsr      sr_CallTIBS43
117300120926      *
117400040729     c                   endsl
117500011109      *
117600011109      * Aggiorna sfile
117700011109     c                   EXSR      AggSfl                                       *aggiorna sfile
117800011107     c                   endif
117900011128      *
118000011128      * testo il flag per la richiesta forzata di fine lettura readc
118100011128      * su sfl, poichè provengo da richiesta F03 del tibs31r
118200011128      * se presenti eventuali altre opzioni.
118300011128     C     $leave        ifeq      *on
118400011220     C                   move      *off          $leave            1
118500011128     C                   leave
118600011128     C                   endif
118700011109      *
118800011109      * lettura successiva record
118900011203     C                   readc     Bs30s1                                 29
119000011107     C                   enddo                                                  *fine sfile
119100011109      *
119200011107     C                   ENDSR
119300011107      ************************************************************
119400011113      * Controllo testata e posizionamento
119500011107      ************************************************************
119600011109     C     Ctrc1         BEGSR
119700011112      *
119800011113     C                   clear                   C1Msg
119900011130     C                   setoff                                       2823
120000011113     C                   setoff                                       414243
120100011130      * Controllo Rollup
120200011130     C     *in25         ifeq      *on
120300011130     C                   seton                                        2823
120400011130     C                   eval      C1Msg = Msg(5)
120500011130     C                   eval      $carSfl = *on
120600011130     C                   endif
120700011113      *
120800011113      * Controllo Filiale
120900011120     C     VCCFil        ifne      *zeros
121000011120     c                   move      VCCFil        kFil              3 0
121100011203     c     kFil          chain     AzOrg01l                           30
121200011120     C   30              seton                                        2841
121300011120     C   30              eval      C1Msg = Msg(1)
121400011113     C                   endif
121500011113      *
121600011113      * Controllo Funzione
121700011120     C     VCCFun        ifne      *blanks
121800011113     C                   clear                   tibs02ds
121900011113     C                   movel     'C'           t02mod
122000011113     C                   movel     *blanks       t02sif
122100011113     C                   movel     'FUN'         t02cod
122200011120     C                   movel     VCCFun        t02ke1
122300011113     C                   call      'TIBS02R'
122400011113     C                   parm                    KPJBA
122500011113     C                   parm                    TIBS02DS
122600011113     C                   if        t02err <> *blanks
122700011120     C                   seton                                        2842
122800011120     C                   eval      C1Msg = Msg(2)
122900011113     C                   endif
123000011113     C                   endif
123100011113      *
123200011113      * Controllo Livello autorità
123300011120     C     VCCAut        ifne      *blanks
123400011113     C                   clear                   tibs02ds
123500011113     C                   movel     'C'           t02mod
123600011113     C                   movel     *blanks       t02sif
123700011113     C                   movel     'LAT'         t02cod
123800011120     C                   movel     VCCAut        t02ke1
123900011113     C                   call      'TIBS02R'
124000011113     C                   parm                    KPJBA
124100011113     C                   parm                    TIBS02DS
124200011113     C                   if        t02err <> *blanks
124300011120     C                   seton                                        2843
124400011120     C                   eval      C1Msg = Msg(3)
124500011113     C                   endif
124600011113     C                   endif
124700130704      *
124800130704      * Controllo Tipo Modulo
124900130704     c                   IF        VCCmopr <> *blanks
125000130704     c                   clear                   tibs02ds
125100130704     c                   movel     'C'           t02mod
125200130704     c                   movel     *blanks       t02sif
125300130704     c                   movel     'MDP'         t02cod
125400130704     c                   movel     VCCmopr       t02ke1
125500130704     C                   call      'TIBS02R'
125600130704     C                   parm                    KPJBA
125700130704     C                   parm                    TIBS02DS
125800130704     C                   if        t02err <> *blanks
125900130704     C                   seton                                        2844
126000130704     C                   eval      C1Msg = Msg(6)
126100130704     C                   endif
126200130704     C                   endif
126300011203      *
126400011120      * Per riposizionarmi non ci devono essere errori
126500011120      * devo avere record da caricare e non devo essere nel caso
126600011120      * di NO REC.
126700011120     C     *in28         ifeq      *off
126800011112      *
126900011109      *
127000011120     C     VCCUte        ifne      w1Ute
127100011120     C     VCCFil        orne      w1Fil
127200011120     C     VCCFun        orne      w1Fun
127300011120     C     VCCAut        orne      w1Aut
127400110110     C     VCCTxt        orne      w1txt
127500130704     C     VCCmopr       orne      w1mopr
127600011107      *
127700011112     C                   EXSR      Poss1
127800011107     C                   endif
127900011120     C                   endif
128000011109      *
128100011107     C                   ENDSR
128200011107      ************************************************************
128300011107      *  Posizionamento su lista
128400011107      ************************************************************
128500011112     C     Poss1         BEGSR
128600011109      *
128700011109      * Controllo se esistono opzioni attive
128800011128     C                   setoff                                       90
128900011220      *
129000011220      * Segnalo il posizionamento
129100011220     C                   move      *on           $Pos              1
129200011109      *
129300011107     C     wmax          ifgt      0
129400011203     C                   readc     Bs30s1                                 29
129500011120     C     *in29         doweq     *off
129600011126     C     *in90         andeq     *off
129700011109      *
129800011120     C     VSCOpz        ifne      ' '
129900011126     C                   seton                                        90
130000011112     C                   endif
130100011109      * Se rilevata opzione attiva, attivo sflnxtchg
130200011126     C     *in90         ifeq      *on
130300100409     c                   eval      *in39 = (HIDdsc < wDate)
130400011120     C                   move      *on           *in22
130500130704      * reimposto l'utente in rosso
130600130704     c                   IF        redute = '1'
130700130704     c                   eval      *in38 = *on
130800130704     c                   ELSE
130900130704     c                   eval      *in38 = *off
131000130704     c                   ENDIF
131100140709      * reimposto l'utente in blu
131200140709     c                   IF        bluute = '1'
131300140710     c                   eval      *in51 = *on
131400140709     c                   ELSE
131500140710     c                   eval      *in51 = *off
131600140709     c                   ENDIF
131700011107      *
131800011203     C                   update    Bs30s1
131900011112     C                   else
132000011203     C                   readc     Bs30s1                                 29
132100011112     C                   endif
132200011109      *
132300011112     C                   enddo
132400011109      *
132500011112     C                   endif
132600011109      *
132700011126     C     *in90         ifeq      *off
132800011120     C                   movel     VCCUte        w1Ute
132900011120     C                   move      VCCFil        w1Fil
133000011120     C                   move      VCCFun        w1Fun
133100011120    >C                   move      VCCAut        w1Aut
133200110110    >C                   move      VCCTxt        w1Txt
133300130704     c                   eval      w1mopr = VCCmopr
133400011109      *
133500011109      * Richiedo l'inizializzazione sfl
133600011107     C                   move      *on           $carsfl
133700011109      *
133800011126     C                   endif
133900011109      *
134000011107     C                   ENDSR
134100011109      *--------------------------------------------------------------------------------------------*
134200011109      * AggSfl - Aggiorna riga di sfile
134300011109      *--------------------------------------------------------------------------------------------*
134400011109     C     Aggsfl        BEGSR
134500100409      *
134600100409      * Reimpostazione attributi di visualizzazione
134700100409     c                   eval      *in39 = (HIDdsc < wDate)
134800100409      *
134900011107      * aggiorna sfile
135000011120     c                   Z-ADD     RecSf         $RecSf                         *posizionamento
135100011120     c                   SETON                                        22
135200011120     c                   clear                   VSCOpz
135300130704      * reimposto l'utente in rosso
135400130704     c                   IF        redute = '1'
135500130704     c                   eval      *in38 = *on
135600130704     c                   ELSE
135700130704     c                   eval      *in38 = *off
135800130704     c                   ENDIF
135900140709      * reimposto l'utente in blu
136000140709     c                   IF        bluute = '1'
136100140710     c                   eval      *in51 = *on
136200140709     c                   ELSE
136300140710     c                   eval      *in51 = *off
136400140709     c                   ENDIF
136500011107      *
136600011203     C                   update    Bs30s1
136700011128     c                   setoff                                       22
136800011109      *
136900011107     C                   ENDSR
137000011109      *--------------------------------------------------------------------------------------------*
137100011109      * RieS1  - Valorizzazione dei campi del subfile
137200011109      *--------------------------------------------------------------------------------------------*
137300011109     C     Ries1         BEGSR
137400120402      *
137500120402     c                   clear                   BS30S1
137600100114      *
137700100114      * - Impostazione flag (NON di abilitazioni)
137800100114     c                   eval      dUte02 = UTEfa1
137900130930     c                   eval      dUte01 = UTEfaf
138000011109      *
138100011112      * Profilo
138200011120     C                   eval      VSCUte  =  UteUte
138300130703
138400011112      * Data scadenza
138500100409     c                   eval      HIDdsc  =  UTEdsc
138600011120     C                   reset                   WLBDA8
138700020329     C                   Z-ADD     Utedsc        G08INV
138800011120     C                   Z-ADD     *zeros        G08DAT
138900011120     C                   move      '3'           G08ERR
139000011120     C                   CALL      'XSRDA8'
139100011120     C                   PARM                    WLBDA8
139200011120      *
139300011120     C     G08err        ifeq      '0'
139400020329     C                   move      G08Dat        VSCdsc
139500011120     C                   endif
139600011112      * P.O.
139700011120     C                   move      UteFil        VSCFil
139800100416      *
139900100416      * flag di Profilo presente sulla tabella "AUT"
140000130930     c                   clear                   vscTBaut
140100130930     c                   if        §UTEAUTOR <> 'NO' and §UTEAUTOR <> *blank
140200130930     c                   move      'S'           vscTBaut
140300130930     c                   end
140400100416      *
140500011112      * Codice utente
140600020329     C                   move      UteCdu        VSCCdu
140700011112      * Funzione
140800130709     c                   eval      hidfun = UTEfun
140900060512     c* sostituito con s.informativo per sapere se si tratta di profilo che
141000060512     c*  si collega in sede o filiale
141100060512     c     uteute        chain     kpprf01l
141200060512     c                   if        %found(kpprf01l)
141300060512     c                   eval      vscsif=prfnsi
141400110111      * Descrizione
141500110111     c                   eval      vsctxt  =  prftxt
141600060512     c                   else
141700060512     c                   eval      vscsif='NoPresente'
141800110111      * Descrizione
141900110111     c                   clear                   vsctxt
142000060512     c                   endif
142100100114      * Modulo Profilo
142200100114     c                   eval      VSCmopr = §UT2mopr
142300100409      *
142400120213     C                   EVAL      vscProj = GetProfiloUtenteProj(vscUte)
142500100409      * Impostazione attributi di visualizzazione
142600100409     c                   eval      *in39 = (HIDdsc < wDate)
142700140528
142800140528      /free
142900140528       *in38 = *off;
143000140710       *in51 = *off;
143100140528       clear xx;
143200140528
143300140528       // se non è un utente STANDARD verifico se ha differenze con il suo STD
143400140528       // i controlli NON li faccio se utente Scaduto
143500140709       // particolarità di UTENTE STD con funzione aziendale COCOM - RED - REA
143600140528       // particolarità di UTENTE STD per filiali messaggerie
143700140528       IF  §UT2mopr <> 'S' and not *in39;
143800140528
143900140528         // se funzione NO COCOM prendo i primi 3 dell'utente per cercare il suo STD
144000140709         // stessa cosa per funzione NO RED e REA
144100140709         IF  UTEfun <> 'COCOM' and UTEfun <> 'RED' and UTEfun <> 'REA';
144200140528
144300140528           // se non è filiale utente messaggeria di area devo aggiungere una 'M'
144400140528           // al profilo per cercare il profilo STD delle messaggerie standard
144500140528           wrkUTE = %subst(VSCute:1:3);
144600140528           IF  IsFilMess;
144700140528             wrkUTE = %trim(wrkUTE) + %trim('M');
144800140528           ENDIF;
144900140528           xx = %lookup(wrkUTE:skUTE);
145000140528         ELSE;
145100140528         // se funzione COCOM devo cercare lo STD COCOM
145200140709           IF  UTEfun = 'COCOM';
145300140528           xx = %lookup('COCOM':skUTE);
145400140710           *in51 = *on;
145500140709           ENDIF;
145600140710         // se funzione RED devo cercare lo STD RDRED
145700140709           IF  UTEfun = 'RED';
145800140710             xx = %lookup('RDRED':skUTE);
145900140710             *in51 = *on;
146000140709           ENDIF;
146100140710         // se funzione REA devo cercare lo STD RAREA
146200140710           IF  UTEfun = 'REA';
146300140710             xx = %lookup('RAREA':skUTE);
146400140710             *in51 = *on;
146500140710           ENDIF;
146600140528         ENDIF;
146700140528         // Devo controllare il campo UTEFAF (DUTE01) senza i campi
146800140528         // §UTEAUTOR e §UTEDSAUT (UTEAUTDS)
146900140528         IF  xx > 0 and
147000140528            (%subst(skABI(xx):1:58) <> %subst(UTEfaf:1:58) or
147100140528             %subst(skABI(xx):96:%len(UTEfaf)-96+1) <>
147200140528             %subst(UTEfaf:96:%len(UTEfaf)-96+1) or
147300140528             skFUN(xx) <> UTEfun or
147400140528             skAUT(xx) <> UTEaut);
147500140528           *in38 = *on;
147600140528         ENDIF;
147700140528         // se utente con S.I. GAITRA201 e i primi 3 byte = allo STD
147800140528         // non devo evidenziare le differenze
147900140528         IF  *in38 and VSCsif = 'GAITRA201 ' and
148000140528             %subst(skute(xx):1:3) = %subst(VSCute:1:3);
148100140528           *in38 = *off;
148200140528         ENDIF;
148300140528       ENDIF;
148400140528      /end-free
148500140528
148600011107     C                   ENDSR
148700011109      *--------------------------------------------------------------------------------------------*
148800011120      *   Routine iniziale
148900011109      *--------------------------------------------------------------------------------------------*
149000011122     C     InzVar        BEGSR
149100011109      *
149200011109      * Ricevimento parametri
149300011107     C     *ENTRY        PLIST
149400011108     C                   PARM                    kpjba
149500020820      *
149600100416     C     Tbe01L        Klist
149700100416     c                   Kfld                    TbeCOD
149800100416     c                   Kfld                    TbeKE1
149900161018      *
150000161018      * -?Verifica del sistema AS/400 corrente?
150100161018     c                   if        UBRTVNETA_Rtv(currSysNeta) <> *zero
150200161018     c                   eval      $Fine = 'S'
150300161018     c                   leavesr
150400161018     c                   endif
150500100416      *
150600020820      * Reperisco dati job
150700020820     C                   EXSR      DatiJob
150800130710
150900130710      * Controllo se richiamato
151000130710     c                   IF        %subst(kpjbu:1:3) <> *blanks and
151100130710     c                             %subst(kpjbu:1:3) <> *zeros
151200130710      * se richiamato vado subito al subfile
151300130710     c                   eval      VCCfil = %dec(%subst(kpjbu:1:3):3:0)
151400130710     c                   eval      TipVid = '2'
151500130710     c                   eval      $carsfl = *on
151600130710     c                   ENDIF
151700020820      *
151800011109     C                   ENDSR
151900130703
152000130703      /free
152100130705       //--------------------------------------------------------------
152200130705       //?Copia da profilo STD devo richiedere filiale dell'utente.    ?
152300130705       //--------------------------------------------------------------
152400130705       BEGSR  GESW03;
152500130705
152600130705         clear W03fil;
152700130705         clear W03dfil;
152800130705         clear wFIL;
152900130705
153000130705         DOW  not *inkl and not *inkf;
153100130705           exfmt BS30W03;
153200130705           clear W03msg;
153300130705           *in28 = *off;
153400130705           SELECT;
153500130705         //?F12 = Ritorno;
153600130705             WHEN  *inkl;
153700130705         //?F06 = Conferma;
153800130705             WHEN  *inkf;
153900130705               exsr ctrFIL;
154000130705               IF  not *in28;
154100130709           //?se sto copiando uno STD NO COCOM passo l'utente così com'è
154200140710           //?stessa cosa per STD NO RDRED e REDREA
154300140710                 IF  HIDfun <> 'COCOM' and
154400140710                     HIDfun <> 'RED' and
154500140710                     HIDfun <> 'REA';
154600130709                   wUTE = VSCute;
154700130709                 ELSE;
154800130709           //?se sto copiando uno STD COCOM devo passare COM come utente
154900140710                   IF  HIDfun = 'COCOM';
155000130709                   wUTE = 'COM';
155100140710                   ENDIF;
155200140710             //?se sto copiando uno STD RDRED devo passare RED come utente
155300140710                   IF  HIDfun = 'RED';
155400140710                     wUTE = 'RED';
155500140710                   ENDIF;
155600140710             //?se sto copiando uno STD RAREA devo passare REA come utente
155700140710                   IF  HIDfun = 'REA';
155800140710                     wUTE = 'REA';
155900140710                   ENDIF;
156000130709                 ENDIF;
156100130705                 wFIL = W03fil;
156200130705                 exsr calltibs31;
156300130705               ENDIF;
156400130705             OTHER;
156500130705         //?ENTER
156600130705               exsr ctrFIL;
156700130705           ENDSL;
156800130705         ENDDO;
156900130705
157000130705       ENDSR;
157100160819
157200160819       //--------------------------------------------------------------
157300160822       //?Copia da profilo STD: devo richiedere filiale dell'utente    ?
157400160819       //?(e non solo...)                                              ?
157500160819       //--------------------------------------------------------------
157600160819       BEGSR  sr_GesWO3;
157700160822
157800160822         //*if  knmus <> 'EDPAC'  and
157900160822         //*    knmus <> 'EDPND'  and
158000160822         //*    knmus <> 'EDPGA'  and
158100160822         //*    knmus <> 'EDPMB'  and
158200160822         //*    knmus <> 'EDPLB';
158300160822         //*  *in28 = *on;
158400160822         //*  V1Msg = Msg(??);
158500160822         //*  leavesr;
158600160822         //*endif;
158700160822
158800160901         // -?"Costruzione" password?
158900160901         exsr  sr_NewPsw;
159000160901
159100160901         // -?Impostazione dati a video?
159200160819         clear  BS30WO3;
159300160901         W03utecpy = VSCute;
159400160819         W03sts = '*ENABLED ';
159500160819         W03scp = '*YES';
159600160901         W03psw = wPassword;
159700160819
159800160901         // -?Ciclo di emissione window WO3?
159900160822         SaveTipVid = TipVid;
160000160819         TipVid = '3';
160100160819
160200160819         DoW  TipVid = '3';
160300160819
160400160819           exfmt  BS30WO3;
160500160819
160600160819           clear  V1msg;
160700160819           *in28 = *off;
160800160819           *in45 = *off;
160900160819           *in46 = *off;
161000160819           *in47 = *off;
161100160819           *in48 = *off;
161200160819           *in49 = *off;
161300160822           *in50 = *off;
161400160819
161500160819           // -?F12 = Ritorno?
161600160819           if  *inKL;
161700160822             TipVid = SaveTipVid;
161800160819             leave;
161900160819           endif;
162000160819
162100160819           // -?Controllo Filiale per cui creare il profilo?
162200160819           exsr  CtrFIL;
162300160819           if  *in28;
162400160819             *in45 = *on;
162500160822             V1msg = W03msg;
162600160819             iter;
162700160819           endif;
162800160819           // -?Controllo Modulo Profilo?
162900160819           exsr  sr_CtrlModPrf;
163000160819           if  *in28  or  *in46;
163100160819             iter;
163200160819           endif;
163300160822           // -?Descrizione MB/AS?
163400160822           if  W03txt =  *blank;
163500160822             *in28 = *on;
163600160822             *in47 = *on;
163700160822             V1Msg = Msg(08);
163800160822             iter;
163900160822           endif;
164000160819           // -?Controllo Stato?
164100160822           if  W03sts <> '*ENABLED '  and
164200160822               W03sts <> '*DISABLED';
164300160819             *in28 = *on;
164400160822             *in48 = *on;
164500160822             V1Msg = Msg(09);
164600160822             iter;
164700160819           endif;
164800160819           // -?Controllo Scadenza Password?
164900160819           if  W03scp <> '*NO '  and
165000160822               W03scp <> '*YES';
165100160819             *in28 = *on;
165200160822             *in49 = *on;
165300160822             V1Msg = Msg(10);
165400160822             iter;
165500160819           endif;
165600160819           // -?Controllo Password?
165700160901           if  W03psw = *blank;
165800160819             *in28 = *on;
165900160822             *in50 = *on;
166000160822             V1Msg = Msg(11);
166100160822             iter;
166200160819           endif;
166300160819
166400160819           // -?F6 = Conferma?
166500160819           if  NOT *inKF;
166600160819             iter;
166700160819           endif;
166800160819
166900160819           Select;
167000160819             // -?SE sto copiando uno STD NO COCOM passo l'utente così com'è?
167100160819             //  ?stessa cosa per STD NO RDRED e REDREA?
167200160819             When  HIDfun <> 'COCOM'  and
167300160819                   HIDfun <> 'RED'    and
167400160819                   HIDfun <> 'REA';
167500160819               wUTE = VSCute;
167600160819             // -?SE sto copiando uno STD COCOM devo passare COM come utente?
167700160819             When  HIDfun = 'COCOM';
167800160819               wUTE = 'COM';
167900160819             // -?SE sto copiando uno STD RDRED devo passare RED come utente?
168000160819             When  HIDfun = 'RED';
168100160819               wUTE = 'RED';
168200160819             // -?SE sto copiando uno STD RAREA devo passare REA come utente?
168300160819             When  HIDfun = 'REA';
168400160819               wUTE = 'REA';
168500160819           EndSl;
168600160819
168700160819           // -?Richiamo *pgm di manutenzione?
168800160819           wFIL = W03fil;
168900160822           clear  TIBS31ds2;
169000160822           TIBS31ds2.I31moPr = W03moPr;
169100160822           TIBS31ds2.I31txt  = W03txt;
169200160822           TIBS31ds2.I31sts  = W03sts;
169300160822           TIBS31ds2.I31scp  = W03scp;
169400160822           TIBS31ds2.I31psw  = W03psw;
169500160819           exsr  CallTIBS31;
169600160822
169700160822           //*if  O31Err = *blank;
169800160822           //*  $CarSfl = '1';
169900160822           //*endif;
170000160822
170100160822           TipVid = SaveTipVid;
170200160822           leave;
170300160819
170400160819         EndDo;
170500160819
170600160819       ENDSR;
170700130705
170800130705       //--------------------------------------------------------------
170900130705       //?Controllo la filiale utente.
171000130705       //--------------------------------------------------------------
171100130705       BEGSR  CTRFIL;
171200130705
171300130705         clear W03dfil;
171400130705
171500130705         IF  W03fil = *zeros;
171600130705           W03msg = 'Immettere la filiale';
171700130705           *in28 = *on;
171800130705           leavesr;
171900130705         ENDIF;
172000130705
172100130705         chain W03fil AZORG01L;
172200130705         IF  not %found(AZORG01L) or ORGfva <> *blanks or
172300130705             (ORGfag <> 'F' and ORGfag <> 'A');
172400130705           W03msg = 'Filiale non valida';
172500130705           *in28 = *on;
172600130705           leavesr;
172700130705         ENDIF;
172800130705
172900130705         W03dfil = ORGdes;
173000130705
173100130705       ENDSR;
173200160819
173300160819       //--------------------------------------------------------------
173400160819       //?Controllo Modulo Profilo.                                    ?
173500160819       //--------------------------------------------------------------
173600160819       BEGSR  sr_CtrlModPrf;
173700160819
173800160819         Select;
173900160819
174000160823           // -?Non inserito nulla?
174100160823           When  W03moPr = *blank;
174200160823             //-*in28 = *on;
174300160823             //-*in46 = *on;
174400160823             //-V1Msg = Msg(06);
174500160823             //-leavesr;
174600160823             W03moPrD = *all'? ';
174700160819
174800160819           // -?Ricerca?
174900160822           When  %scan( '?' : W03moPr ) > *zero;
175000160822             clear  W03moPr;
175100160819             clear  TIBS02ds;
175200160819             T02mod = 'R';
175300160819             T02cod = 'MDP';
175400160819      /end-free
175500160819     C                   CALL      'TIBS02R'
175600160819     C                   PARM                    kpjba
175700160819     C                   PARM                    TIBS02ds
175800160819      /free
175900160819             if  T02err = *blank;
176000160822               W03moPr = T02ke1;
176100160819             endif;
176200160819             *in46 = *on;
176300160819             leavesr;
176400160819
176500160819           // -?Controllo?
176600160819           Other;
176700160819             clear  TIBS02ds;
176800160819             T02mod = 'C';
176900160819             T02cod = 'MDP';
177000160822             T02ke1 = W03moPr;
177100160819      /end-free
177200160819     C                   call      'TIBS02R'
177300160819     C                   parm                    KPJBA
177400160819     C                   parm                    TIBS02DS
177500160819      /free
177600160819             if  T02err <> *blank;
177700160819               *in28 = *on;
177800160819               *in46 = *on;
177900160819               V1Msg = Msg(06);
178000160819               leavesr;
178100160819             endif;
178200160822             W03moPrD = T02uni;
178300160822             if  W03moPr = 'S';
178400160822               *in28 = *on;
178500160822               *in46 = *on;
178600160822               V1Msg = %trimR( Msg(06) ) + ' - non inseribile';
178700160822               leavesr;
178800160822             endif;
178900160819
179000160819         EndSl;
179100160819
179200160819       ENDSR;
179300160822
179400160822       //--------------------------------------------------------------
179500160822       //?Creazione nuova password per l'utente in copia.
179600160822       //--------------------------------------------------------------
179700160822       BEGSR  sr_NewPsw;
179800160822
179900160822         clear  wPassword;
180000160822
180100160901         // ·?1° carattere = lettera reperita in base alla 1ª del?
180200160901         //  ?               nominativo?
180300160901         wP  = %scan( %subst( %xlate( c_Minusc : c_Maiusc : VSCtxt )
180400160901                              : 1 : 1 ) : c_Maiusc );
180500160822         wP += 8;
180600160822         if  wP > %elem( sk_alfab );
180700160822           wP = %rem( wP : %elem( sk_alfab ) );
180800160822         endif;
180900160822         wPassword = sk_alfab(wP);
181000160901
181100160901         // ·?2° carattere = lettera reperita in base alla 2ª del?
181200160901         //  ?               nominativo?
181300160901         wP  = %scan( %subst( %xlate( c_Minusc : c_Maiusc : VSCtxt )
181400160901                              : 2 : 1 ) : c_Maiusc );
181500160822         wP -= 8;
181600160822         select;
181700160822           when  wP < 1;
181800160822             wP += %elem( sk_alfab );
181900160822           when  wP > %elem( sk_alfab );
182000160901             wP  = %rem( wP : %elem( sk_alfab ) );
182100160822         endsl;
182200160901         wPassword = %trimr( wPassword ) + sk_alfab(wP);
182300160901
182400160822         // ·?3°-5° carattere = millisecondi attuali?
182500160822         //        ?(eventuale sostituzione della prima cifra SE *zero)?
182600160822         // ·?6°-9° carattere = secondi attuali elevati al quadrato?
182700160901         wP  = ( %subdt( %timestamp() : *ms ) * 10 ) +
182800160901               ( %subdt( %timestamp() : *s ) *
182900160901                 %subdt( %timestamp() : *s ) );
183000160822         if  wP  < 1000000;
183100160822           wP += 9000000;
183200160822         endif;
183300160901         wPassword = %trimr( wPassword ) + %editc( wP : 'X' );
183400160822
183500160822       ENDSR;
183600130705
183700130703       //--------------------------------------------------------------
183800130703       //?Carico abilitazioni per profili standard.                    ?
183900130703       //--------------------------------------------------------------
184000130703       BEGSR  ABISTD;
184100130703
184200130704         wEnd = *off;
184300130703         clear xx;
184400130703         clear skUTE;
184500130703         clear skABI;
184600130703         clear skAUT;
184700130703         clear skFUN;
184800130703
184900130703       //?Carico gli utenti standard in SK + abilitazioni
185000130703         exec sql
185100130703         DECLARE AZUTE cursor for
185200130703         SELECT * from AZUTE00F
185300130703         ORDER BY UTEute;
185400130703
185500130703       //?Apertura del cursore
185600130703         exec sql
185700130703         OPEN AZUTE;
185800130703
185900130703         IF  sqlcode < 0;
186000130703           wEnd = *on;
186100130703         ENDIF;
186200130703
186300130703         DOW  not wEnd;
186400130703           exec sql
186500130703           FETCH next from AZUTE into :azuteds;
186600130703           IF  sqlcod = 100 or sqlcod < 0;
186700130703             wEnd = *on;
186800130703             leave;
186900130703           ENDIF;
187000130703
187100130703       //?solo utenti standard
187200130703           dUte02 = UTEfa1;
187300130930           dUte01 = UTEfaf;
187400130703           IF  §UT2mopr = 'S';
187500130703             xx += 1;
187600130703             skUTE(xx) = UTEute;
187700130703             skABI(xx) = UTEfaf;
187800130703             skAUT(xx) = UTEaut;
187900130703             skFUN(xx) = UTEfun;
188000130703           ENDIF;
188100130703
188200130703         ENDDO;
188300130703
188400130703       //?Chiusura del cursore
188500130703         exec sql
188600130703         CLOSE AZUTE;
188700130703
188800130703       ENDSR;
188900161018
189000161018       //--------------------------------------------------------------
189100161018       //?Copia da profilo utente: devo richiedere una password di     ?
189200161018       //?autorizzazione                                               ?
189300161018       //--------------------------------------------------------------
189400161018       BEGSR  sr_GesW04;
189500161018
189600161018         // -?Impostazione dati a video?
189700161018         clear  BS30W04;
189800161018         W04uteCpy  = VSCute;
189900161018
190000161018         // -?Ciclo di emissione window W04?
190100161018         SaveTipVid = TipVid;
190200161018         TipVid = '4';
190300161018
190400161018         DoW  TipVid = '4';
190500161018
190600161018           exfmt  BS30W04;
190700161018
190800161018           clear  V1msg;
190900161018           *in28 = *off;
191000161018           *in50 = *off;
191100161018
191200161018           // -?F12 = Ritorno?
191300161018           if  *inKL;
191400161018             TipVid = SaveTipVid;
191500161018             leave;
191600161018           endif;
191700161018
191800161018           // -?Controllo Password per copiare il profilo?
191900161018           select;
192000161018             when  W04psw = *blank;
192100161018               *in28 = *on;
192200161018               *in50 = *on;
192300161018               V1Msg = Msg(12);
192400161018               iter;
192500161018             //*·when  W04psw <> c_CopyPwd;
192600161018             when  W04psw <> SDSpgm;
192700161018               *in28 = *on;
192800161018               *in50 = *on;
192900161018               V1Msg = Msg(13);
193000161018               iter;
193100161018           endsl;
193200161018
193300161018           //*·// -?F6 = Conferma?
193400161018           //*·if  NOT *inKF;
193500161018           //*·  iter;
193600161018           //*·endif;
193700161018
193800161018           // -?Richiamo *pgm di manutenzione?
193900161018           TipVid = SaveTipVid;
194000161018           wUTE = W04uteCpy;
194100161018           clear  TIBS31ds2;
194200161018           exsr  CallTIBS31;
194300161018
194400161018           leave;
194500161018
194600161018         EndDo;
194700161018
194800161018       ENDSR;
194900130703
195000130703      /end-free
195100130703
195200020820      *----------------------------------------------------------------
195300020820      *  Reperisco Dati del job (Utente/Operativi)
195400020820      *----------------------------------------------------------------
195500020820     C     DatiJob       BegSr
195600020820      *
195700020820     C     *dtaara       define    §azute        azuteds
195800020820     C     *dtaara       define    §datiute      ddatiute
195900020820      *
196000020820     C                   in(E)     *dtaara
196100020820     C                   IF        %Error or RSUT = *blanks
196200020820     C                   call      'TIBS34R'
196300020820     C                   parm                    Tibs34Ds
196400020820     C                   in        *dtaara
196500020820     C                   ENDIF
196600020820      *
196700020820     C                   endsr
196800011109      *--------------------------------------------------------------
196900120213
197000120213     P*--------------------------------------------------
197100120213     P* Procedure name: GetProfiloUtenteProj
197200120213     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
197300120213     P* Returns:        Profilo utente in Proj.
197400120213     P* Parameter:      priProfiloUtente => Profilo Utente
197500120213     P*--------------------------------------------------
197600120213     P GetProfiloUtenteProj...
197700120213     P                 B
197800120213     D GetProfiloUtenteProj...
197900120213     D                 PI             1A
198000120213     D  priProfiloUtente...
198100120213     D                               10A
198200120213
198300120213     D retField        S              1A   STATIC
198400120213
198500120213      /FREE
198600120213
198700120213       CLEAR retField;
198800120213
198900120213       EXEC SQL
199000120217         SELECT CASE
199100120217                WHEN utnAnn = '1' THEN 'A'
199200120217                WHEN utnGrp = 999 THEN 'D'
199300120217                ELSE 'S'
199400120217                END
199500120213           INTO :retField
199600120213           FROM anutn00f
199700120213           WHERE utnPrf = :priProfiloUtente
199800120213       ;
199900120213
200000120213       SELECT;
200100120213         WHEN sqlCode < *ZERO;
200200120213           DUMP(A);
200300120213           retField = '?';
200400120213       ENDSL;
200500120213
200600120213       RETURN retField;
200700120213
200800120213      /END-FREE
200900120213     P GetProfiloUtenteProj...
201000120213     P                 E
201100140522
201200140522      *----------------------------------------------------
201300140523      * Procedure name: IsFilMess
201400140523      * Purpose:        Filiale utente Messaggeria
201500140522      * Returns:        ON - OFF
201600140522      *----------------------------------------------------
201700140523     p IsFilMess       B
201800140523     d IsFilMess       PI              n
201900140522
202000140522     d isErrore        s               n   inz
202100140522
202200140522      /free
202300140522
202400140522       clear OG140;
202500140522       clear OG143;
202600140522       chain (UTEfil) AZORG01L;
202700140522       IF  %found(AZORG01L);
202800140522         OG140 = ORGde0;
202900140522         OG143 = ORGde3;
203000140522       ENDIF;
203100140522
203200140523       IF og143.§OGntw = 'MES' and og140.§OGapo <> %editc(ORGfil:'X');
203300140522         return *on;
203400140522       ELSE;
203500140522         return *off;
203600140522       ENDIF;
203700140522
203800140522      /end-free
203900140523     p IsFilMess       E
204000140522
204100160822** - sk_alfab +....2....+.
204200160822ABCDEFGHIJKLMNOPQRSTUVWXYZ
204300160822** -- MSG - Lungh. 78 -------------------------------------------------------*
204400130709TIBS30R- Filiale inesistente
204500011109TIBS30R- Funzione non corretta
204600011109TIBS30R- Autorità non corretta
204700011109TIBS30R- Opzione non corretta
204800011130Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.
204900130704TIBS30R- Tipo Modulo non corretto
205000130704TIBS30R- Immettere la filiale se richiesto Ordinamento per Cod.Utente
205100160822TIBS30R- Descrizione obbligatoria                                              8
205200160822TIBS30R- Stato del profilo non corretto (*ENABLED/*DISABLED)                   9
205300160822TIBS30R- Scadenza Password non corretta (*NO/*YES)                            10
205400160822TIBS30R- Password del profilo obbligatoria                                    11
205500161018TIBS30R- Immettere la password                                                12
205600161018TIBS30R- Password errata                                                      13
