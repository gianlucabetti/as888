000100011112     H DECEDIT('0,') DATEDIT(*YMD.)
000200120213     H DFTACTGRP(*NO)
000300011112      *****************************************************************
000400011112      *
000500011112      * Gestione Profili utente
000600011112      *
000700011112      *****************************************************************
000800980521      *  RIEPILOGO INDICATORI
000900981008      ****************************************************************
001000011129      *
001100011120      * 20    - SflDsp
001200011120      *N21    - SflClr
001300011120      * 21    - SflDspCtl
001400011120      * 22    - SflNxtChg
001500011130      * 23    - Lettura ultimo record
001600011108      * 24    - Nessun record trovato
001700011120      * 25    - Rollup
001800011120      * 28    - Emissione messaggio di errore
001900011120      * 29    - Readc su sfl
002000130704      * 38    - In rosso se profilo ha AUT diverse da Profilo STD
002100100409      * 39    - Evidenziazione profilo scaduto nel subfile
002200011109      * 40    - Errore su opzione (sfl)
002300011203      * 41    - Errore su filiale          (prima videata e control)
002400011203      * 42    - Errore su funzione         (prima videata e control)
002500011203      * 43    - Errore su livello autorità (prima videata e control)
002600130704      * 44    - Errore su tipo modulo      (prima videata e control)
002700140710      * 51    - In blu se profilo con funzione aziendale COCOM - REA - RED
002800990317      ****************************************************************
002900011109     FAzUte01l  IF   E           K DISK
003000100416     FTnTbe01l  IF   E           K DISK
003100011112     FAzOrg01l  IF   E           K DISK
003200060512     Fkpprf01l  IF   E           K DISK
003300011203     FTibs30d   CF   E             WORKSTN  sfile(BS30S1:RecSf)
003400980521      *------------------------------------------------------------------------*
003500160822      *__________
003600160822      * Costanti
003700160822      *¯¯¯¯¯¯¯¯¯¯
003800160822      * ?set caratteri Maiuscoli/Minuscoli
003900160822     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004000160822     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
004100160822       // -?Caratteri minuscoli?
004200160822     d c_Minusc        c                   const('abcdefghijklm+
004300160822     d                                            nopqrstuvwxyz+
004400160822     d                                            àáâãäå+
004500160822     d                                            æç+
004600160822     d                                            èéêë+
004700160822     d                                            ìíîï+
004800160822     d                                            òóôõö+
004900160822     d                                            ùúûü')
005000160822
005100160822       // -?Caratteri maiuscoli?
005200160822     d c_Maiusc        c                   const('ABCDEFGHIJKLM+
005300160822     d                                            NOPQRSTUVWXYZ+
005400160822     d                                            ÀÁÂÃÄÅ+
005500160822     d                                            ÆÇ+
005600160822     d                                            ÈÉÊË+
005700160822     d                                            ÌÍÎÏ+
005800160822     d                                            ÒÓÔÕÖ+
005900160822     d                                            ÙÚÛÜ')
006000160822      *_________________________________
006100160822      * Schiere a tempo di compilazione
006200160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
006300160822       // -?Caratteri alfabetici?
006400160822     d sk_alfab        s              1    dim(26) ctdata perrcd(26)
006500160822
006600160822       // -?Messaggi di errore?
006700160822     D Msg             S             78    DIM(11) CTDATA PERRCD(1)
006800130703
006900160822      *_______________________________
007000160822      * Schiere a tempo di esecuzione
007100160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
007200130703       // -?Schiere per abilitazioni profili standard
007300130703     d skUTE           s             10    dim(999)
007400130703     d skABI           s            500    dim(999)
007500130703     d skAUT           s              2    dim(999)
007600130703     d skFUN           s              5    dim(999)
007700130703
007800160822      *___________
007900160822      * Variabili
008000160822      *¯¯¯¯¯¯¯¯¯¯¯
008100011109     D $Fine           S              1    INZ('N')                             *fine programma
008200011109     D TipVid          S              1    INZ('1')                             *video in gestione
008300160822     D SaveTipVid      s              1    inz                                  *video in gestione
008400011109     D $CarD1          S              1    INZ('1')                             *video in gestione
008500011112     D $CarSfl         S              1    INZ('0')                             *video in gestione
008600011220     D $Leave10        S              1    INZ('0')                             *video in gestione
008700130704     d wFIL            s                   like(V1CFil)
008800130705     d wUTE            s                   like(V1Cute)
008900011112     D W1Ute           S                   like(V1CUte)
009000011112     D W1Fil           S                   like(V1CFil)
009100011112     D W1Fun           S                   like(V1CFun)
009200011112     D W1Aut           S                   like(V1CAut)
009300110110     D W1txt           S                   like(V1Ctxt)
009400130704     d w1mopr          s                   like(V1Cmopr)
009500011120     D W2Ute           S                   like(V1CUte)
009600011120     D W2Fil           S                   like(V1CFil)
009700011120     D W2Fun           S                   like(V1CFun)
009800011120     D W2Aut           S                   like(V1CAut)
009900110110     D W2txt           S                   like(V1CTxt)
010000130704     d w2mopr          s                   like(V1Cmopr)
010100160822     d wPassword       s                   like(W03psw)  inz
010200110110     d wprftxt         s                   like(prftxt)
010300110110     d wrk2txt         s             44    varying
010400011120     d RecSf           S              4  0                                      *n° rel sf
010500011120     D WMax            S                   like($RecSf)
010600011112     D Win             S             99A
010700100409     d wDate           s              8  0 inz
010800100409     d wDate_Iso       s               d   inz  datfmt(*iso)
010900140522     d wrkUTE          s             10    inz
011000160822     d wP              s              7  0 inz
011100130703
011200130703       // -?Flags booleani
011300130703     d wEnd            s               n   inz(*off)
011400130703
011500130703       // -?Indici di schiera
011600130703     d xx              s              4  0 inz
011700130704     d yy              s              4  0 inz
011800130703
011900160822      *________________
012000160822      * Strutture dati
012100160822      *¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
012200160822     D kpjba         E DS
012300160822     D Tibs31ds      e ds                  inz
012400160822     D Tibs31ds2     e ds                  inz   qualified
012500160822     D Tibs32ds      e ds                  inz
012600160822     D Tibs02Ds      E DS
012700160822     D*m TntbAUTds     E DS
012800011120      *-------------
012900011120     D WLBDA8          DS                  INZ
013000011120     D  G08DAT                 1      8  0
013100011120     D  G08INV                 9     16  0
013200011120     D  G08ERR                17     17
013300011120     D  G08TGI                18     22  0
013400020820      *-----------------------------------------------------------
013500020820      * Ds di riferimento al file esterno AzUte00F
013600020820      *--------
013700020820     D AzuteDs       E DS                  ExtName(AzUte00F)
013800100114
013900100114       // - DS per flags (vedi "Modulo Profilo")
014000100114     d dUte02        e ds                  inz
014100130930     d dUte01        e ds                  inz
014200020820      *-----------------------------------------------------------
014300020820      * Ds per dati organigramma
014400020820      *--------
014500020820     D DDatiUte      E DS
014600020820      *-----------------------------------------------------------
014700020820      * Parametri per richiamo al pgm di controllo profilo utenti
014800020820      *--------
014900020820     D Tibs34Ds      E DS                  Inz
015000140522
015100140522      * Campo ORGde0 di AZORG00F
015200140522     d OG140         e ds                  inz qualified
015300140522      * Campo ORGde3 di AZORG00F
015400140522     d OG143         e ds                  inz qualified
015500140522
015600160822       //--------------------------------------------------------------
015700160822       //?Definizione procedure usate.
015800160822       //--------------------------------------------------------------
015900120213
016000120213     D*--------------------------------------------------
016100120213     D* Procedure name: GetProfiloUtenteProj
016200120213     D* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
016300120213     D* Returns:        Profilo utente in Proj.
016400120213     D* Parameter:      priProfiloUtente => Profilo Utente
016500120213     D*--------------------------------------------------
016600120213     D GetProfiloUtenteProj...
016700120213     D                 PR             1A
016800120213     D  priProfiloUtente...
016900120213     D                               10A
017000130704
017100130704      //---------------------------------------------------------------
017200130704      //?Ordinamento SBFL
017300130704      //---------------------------------------------------------------
017400130704      // ----------------------------------------------------------------------
017500130704      //?Costanti per ordinamento subfile
017600130704      // ----------------------------------------------------------------------
017700130704     d MaxKey          c                   4
017800130704     d Ascendente      c                   1
017900130704     d Discendente     c                   2
018000130704     d Carattere       c                   6
018100130704     d Numerico        c                   8
018200130704     d Put             c                   1
018300130704     d EndPut          c                   2
018400130704     d Get             c                   3
018500130704      **********************************************************************
018600130704      * Campi utili:
018700130704      *     nrr        - Numero relativo di record del Subfile
018800130704      *     SizeList   - Dimensione della lista
018900130704      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
019000130704      **********************************************************************
019100130704     d NotUsed         s             16A
019200130704     d ReturnSize      s              9B 0
019300130704     d SizeList        s              9B 0
019400130704     d RrnLast         s              5I 0
019500130704     d WrkSort         s              1  0 inz(0)
019600130704      **********************************************************************
019700130704      * Data Structures
019800130704      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
019900130704      *     QLGSCB     - The sort request block for the QLGSORT API
020000130704      *     QLGSCB00   - The sort request block for the QLGSRTIO API
020100130704      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
020200130704      *     QUSEC      - Error structure for the QLGSORT API
020300130704      **********************************************************************
020400130704     d sflrcd          ds
020500130704     d  vscopz
020600130704     d  vscute
020700130704     d  vscproj
020800130704     d  vsctbaut
020900130704     d  vscdsc
021000130704     d  vscfil
021100130704     d  vsccdu
021200130704     d  vsctxt
021300130704     d  vscsif
021400130704     d  vscmopr
021500130704     d  hiddsc
021600130704     d  redute
021700140709     d  bluute
021800130709     d  hidfun
021900130704     d  selected                      1A
022000130704
022100130704      /COPY QSYSINC/QRPGLESRC,QLGSORT
022200130704     d QLGKL                         16    DIM(MaxKey)
022300130704     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
022400130704     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
022500130704     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
022600130704     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
022700130704
022800130704      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
022900130704      /COPY QSYSINC/QRPGLESRC,QUSEC
023000130704
023100130704     d Qlgsort_pr      pr                  extpgm('QLGSORT')
023200130704     d  pr_QLGSCB                          like(Qlgscb)
023300130704     d  pr_NotUsed1                        like(NotUsed)
023400130704     d  pr_NotUsed2                        like(NotUsed)
023500130704     d  pr_SizeList                        like(SizeList)
023600130704     d  pr_ReturnSize                      like(ReturnSize)
023700130704     d  pr_QUSEC                           like(Qusec)
023800130704     d                                     options(*varsize)
023900130704
024000130704     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
024100130704     d  pr_QLGSCB00                        like(QLGSCB00)
024200130704     d  pr_SflRcd                          like(SflRcd)
024300130704     d  pr_NotUsed1                        like(NotUsed)
024400130704     d  pr_SizeList                        like(SizeList)
024500130704     d  pr_NotUsed2                        like(NotUsed)
024600130704     d  pr_QUSEC                           like(Qusec)
024700130704     d                                     options(*varsize)
024800130704
024900130704     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
025000130704     d  pr_QLGSCB00                        like(QLGSCB00)
025100130704     d  pr_NotUsed1                        like(NotUsed)
025200130704     d  pr_SflRcd                          like(SflRcd)
025300130704     d  pr_SizeList                        like(SizeList)
025400130704     d  pr_NotUsed2                        like(NotUsed)
025500130704     d  pr_QUSEC                           like(Qusec)
025600130704     d                                     options(*varsize)
025700120213
025800120213      *------------------------------------------------------------------------*
025900011108      * Main
026000011108      *--------------------------------------------------------------------------------------------*
026100011108      *
026200011122     C                   EXSR      InzVar
026300130703
026400011109      * Gestione video
026500011108     C     $Fine         DOUEQ     'S'
026600011109     C     TipVid        CASEQ     '1'           GesD1                           *Sfile
026700011109     C     TipVid        CASEQ     '2'           GesS1                           *Sfile
026800011107     C                   ENDCS
026900011107     C                   ENDDO
027000011112      *
027100011107     C                   SETON                                        LR
027200011109      *--------------------------------------------------------------------------------------------*
027300011109      * GesD1  - Gestione prima videata di parzializzazione
027400011109      *--------------------------------------------------------------------------------------------*
027500011109     C     GesD1         BEGSR
027600011109      *
027700011109      * Inizializzazione videata
027800011109     C     $CarD1        ifeq      *on
027900011109     C                   EXSR      CarD1
028000011109     C                   move      *off          $CarD1
028100011109     C                   endif
028200011113      *
028300011113      * Scrivo il piede
028400011113     C                   write     Bs30P1
028500011114      *
028600011114      * scrivo il formato se ci sono errori
028700011120     C     *in28         ifeq      *on
028800011114     C                   movea     *In           Win
028900011120     C                   move      *ALL'0'       in2844           17
029000011120     C                   movea     IN2844        *in(28)
029100011114     C                   WRITE     Bs30D1
029200011114     C                   movea     Win           *In
029300011114     C                   endif
029400011109      *
029500011114      * Emetto il formato
029600011113     C                   EXFMT     Bs30D1
029700011109      *
029800011114     C     '?'           Scan      V1CFun                                 45
029900011114     C     '?'           Scan      V1CAut                                 46
030000130704     C     '?'           Scan      V1Cmopr                                49
030100130705     c                   clear                   wFIL
030200011109      *
030300011109      * Ricerca
030400011109     C                   SELECT
030500011109      *
030600011109      * F3=Fine
030700011109     C     *inKc         wheneq    *on
030800011109     C                   EXSR      F03Ges
030900011109      *
031000011114     C     *in45         wheneq    *on
031100011114     C     *in46         oreq      *on
031200130704     C     *in49         oreq      *on
031300011109     C                   EXSR      Search
031400130704
031500130704      * F8=Ordinamento x cod.utente
031600130704     c                   WHEN      *inKH
031700130704     c                   eval      wFIL = V1Cfil
031800130704     c                   EXSR      F08Ges
031900130704     c   28              seton                                        41
032000130704     c   28              eval      V1Msg = Msg(7)
032100130705     c                   clear                   wFIL
032200011109      *
032300011109      * F10= Inserimento
032400011109     C     *inKj         wheneq    *on
032500011109     C                   EXSR      F10Ges
032600011220      *
032700011203     C                   OTHER
032800011109      *
032900011109      * Controlli e decodifiche
033000011109     C                   EXSR      CtrD1
033100011109      *
033200011120     C     *IN28         ifeq      *off
033300011109      *
033400011109      * Gestisco il video di dettaglio
033500011113     C                   move      *on           $carsfl
033600011109     C                   move      '2'           TipVid
033700011120      *
033800011120      * Valorizzo ii campi del control
033900011120     C                   eval      VCCUte = V1CUte
034000011120     C                   eval      VCCFun = V1CFun
034100011120     C                   eval      VCCAut = V1CAut
034200011120     C                   z-add     V1CFil        VCCFil
034300110110     C                   eval      VCCTxt = V1CTxt
034400130704     c                   eval      VCCmopr = V1Cmopr
034500011109     C                   endif
034600011203     C                   ENDSL
034700011109      *
034800011109     C                   endsr
034900011109      *--------------------------------------------------------------------------------------------*
035000011109      * Search - Ricerca su campi video
035100011109      *--------------------------------------------------------------------------------------------*
035200011109     C     Search        BEGSR
035300011120     C                   setoff                                       28
035400011109      *
035500011109      * Funzione
035600011114     C     *in45         ifeq      *on
035700011114     C     *in47         oreq      *on
035800011114     C   45              movel     *blanks       V1CFun
035900011120     C   47              movel     *blanks       VCCFun
036000011109     C                   move      *blanks       Tibs02Ds
036100011114      *
036200011109     C                   move      'FUN'         T02Cod
036300011109     C                   move      'R'           T02Mod
036400011109     C                   movel     KnSif         T02Sif
036500011109     C                   CALL      'TIBS02R'
036600011109     C                   PARM                    kpjba
036700011109     C                   PARM                    Tibs02Ds
036800011109     C                   if        T02Err =  *blanks
036900011114     C   45              movel     T02ke1        V1CFun
037000011120     C   47              movel     T02ke1        VCCFun
037100011109     C                   endif
037200011109     C                   endif
037300011109      *
037400011109      * Autorità
037500011114     C     *in46         ifeq      *on
037600011114     C     *in48         oreq      *on
037700011114      *
037800011114     C   46              movel     *blanks       V1CAut
037900011120     C   48              movel     *blanks       VCCAut
038000011109     C                   move      *blanks       Tibs02Ds
038100011114      *
038200011112     C                   move      'LAT'         T02Cod
038300011109     C                   move      'R'           T02Mod
038400011109     C                   movel     KnSif         T02Sif
038500011109     C                   CALL      'TIBS02R'
038600011109     C                   PARM                    kpjba
038700011109     C                   PARM                    Tibs02Ds
038800011109     C                   if        T02Err =  *blanks
038900011114     C   46              movel     T02ke1        V1CAut
039000011120     C   48              movel     T02ke1        VCCAut
039100011109     C                   endif
039200011109     C                   endif
039300130704      *
039400130704      * Tipo Modulo
039500130704     C     *in49         ifeq      *on
039600130704     C     *in50         oreq      *on
039700130704      *
039800130704     C   49              movel     *blanks       V1Cmopr
039900130704     C   50              movel     *blanks       VCCmopr
040000130704     C                   move      *blanks       Tibs02Ds
040100130704      *
040200130704     C                   move      'MDP'         T02Cod
040300130704     C                   move      'R'           T02Mod
040400130704     C                   movel     KnSif         T02Sif
040500130704     C                   CALL      'TIBS02R'
040600130704     C                   PARM                    kpjba
040700130704     C                   PARM                    Tibs02Ds
040800130704     C                   if        T02Err =  *blanks
040900130704     C   49              movel     T02ke1        V1Cmopr
041000130704     C   50              movel     T02ke1        VCCmopr
041100130704     C                   endif
041200130704     C                   endif
041300011114      *
041400011114     C                   setoff                                       4546
041500011114     C                   setoff                                       4748
041600130704     C                   setoff                                       4950
041700011109     C                   ENDSR
041800011109      *--------------------------------------------------------------------------------------------*
041900011109      * F03Ges - Tasto funzionale F03 -> Fine programma
042000011109      *--------------------------------------------------------------------------------------------*
042100011109     C     F03Ges        BEGSR
042200011109      *
042300011109     C                   Movel     'S'           $Fine                          *fine pgm
042400011109      *
042500011109     C                   ENDSR
042600130704      /free
042700130704       //--------------------------------------------------------------
042800130704       //  F08Ges - Tasto funzionale F08 -> Ordinamento x Cod.Utente
042900130704       //--------------------------------------------------------------
043000130704       BEGSR  F08ges;
043100130704
043200130704       //?Serve la filiale
043300130704         IF  wFIL = 0;
043400130704           *in28 = *on;
043500130704           leavesr;
043600130704         ENDIF;
043700130704
043800130704         rrnlast = wmax;
043900130704
044000130704       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
044100130704       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
044200130704       // il campo è usato per selezionare i records dando un ordine a quelli
044300130704       // selezionati davanti ai non selezionati.
044400130704         clear QLGSCB;
044500130704         clear QLGSCB00;
044600130704
044700130704       // 2 campi chiave x Ordinamento cod.Utente - codice utente + filiale
044800130704         QLGNBRK = 2;
044900130704
045000130704       // imposto la posizione del cod.utente sul subfile e la lunghezza
045100130704       // l'ordinamento è su un campo numerico e deve essere
045200130704       // ascending
045300130704         QLGSP = 1 + %size(VSCopz)+ %size(VSCute) + %size(VSCproj) +
045400130704                     %size(VSCtbaut) + %size(VSCdsc) + %size(VSCfil);
045500130704         QLGSS = %SIZE(VSCcdu);
045600130704         QLGDT = Numerico;
045700130704         QLGSO = Ascendente;
045800130704         QLGKL(1) = QLGSKL;
045900130704
046000130704       // imposto la posizione della filiale  sul subfile e la lunghezza
046100130704       // l'ordinamento è su un campo numerico e deve essere
046200130704       // ascending
046300130704         QLGSP = 1 + %size(VSCopz)+ %size(VSCute) + %size(VSCproj) +
046400130704                     %size(VSCtbaut) + %size(VSCdsc);
046500130704         QLGSS = %SIZE(VSCfil);
046600130704         QLGDT = Numerico;
046700130704         QLGSO = Ascendente;
046800130704         QLGKL(2) = QLGSKL;
046900130704
047000130704       // Load other sort parameters.
047100130704         QLGLB = 80 + 16 * MaxKey;
047200130704         QLGRL = %SIZE(sflrcd) - 1;
047300130704         QLGRT = 8;
047400130704         QLGOKL = 80;
047500130704         QLGLKE = 16;
047600130704         QLGLSS = 290;
047700130704
047800130704       // Initialize Sort I/O API fields.
047900130704         QLGRL00 = QLGRL;
048000130704         QLGRC00 = 1;
048100130704         clear QUSEI;
048200130704         QUSBPRV = %SIZE(QUSEC);
048300130704
048400130704       // First step - Initialize the sort routine.
048500130704         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
048600130704
048700130704       // Next step - Write records to I/O routine.
048800130704         QLGRT00 = Put;
048900130704         for yy = 1 to rrnlast;
049000130704          chain yy BS30S1;
049100130704
049200130704       // solo le righe con Selected = 'Y' sono riordinate,
049300130704       // quindi per fare un ordinamento di tutte le righe
049400130704       // metto 'Y' sempre.
049500130704          selected  = 'Y';
049600130704          clear QUSEI;
049700130704          QUSBPRV = %SIZE(QUSEC);
049800130704          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
049900130704         endfor;
050000130704
050100130704       // Next step - Signal end of input, clear subfile for reload.
050200130704         QLGRT00 = EndPut;
050300130704         clear QUSEI;
050400130704         QUSBPRV = %SIZE(QUSEC);
050500130704         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
050600130704       // pulizia SFL
050700130704         clear recsf;
050800130704         *in20 = *off;
050900130704         *in21 = *off;
051000130704         write BS30C1;
051100130704         *in20 = *on;
051200130704         *in21 = *on;
051300130704
051400130704       // Final step - Write the records back to the subfile.
051500130704         QLGRT00 = Get;
051600130704         for yy = 1 to rrnlast;
051700130704          clear QUSEI;
051800130704          QUSBPRV = %SIZE(QUSEC);
051900130704          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
052000130704          recsf = yy;
052100130704          *in38 = (redute = '1');
052200140710          *in51 = (bluute = '1');
052300130704          *in39 = (HIDdsc < wDate);
052400130704          write BS30S1;
052500130704         endfor;
052600130704
052700130704         // Visualizzazione del SFL (se ci sono dati)
052800130704         *in20 = (recsf > *zeros);
052900130704
053000130704         // Posizionamento cursore al 1° rcd della pagina
053100130704         $recsf = 1;
053200130704
053300130704       ENDSR;
053400130704
053500130704      /end-free
053600130704
053700011109      *--------------------------------------------------------------------------------------------*
053800011109      * F10Ges - Tasto funzionale F10 -> Inserimento
053900011109      *--------------------------------------------------------------------------------------------*
054000011109     C     F10Ges        BEGSR
054100011210      *
054200011203     C                   clear                   Tibs31Ds
054300011203     C                   move      'I'           I31Opz
054400011220     C                   move      *off          $leave10          1
054500011109      *
054600011109      * Lo richiamo nella modalità 'nuovo record'
054700011109     C                   CALL      'TIBS31R'
054800011109     C                   PARM                    kpjba
054900011203     C                   PARM                    Tibs31Ds
055000011120      *
055100011120      *
055200011127      * Se provengo dalla prima videata ricarico la prima videata.
055300011127      * Se provengo da Sfl distinguo i casi con cui sono uscita
055400011203      * da tibs31r : se ho F03 =*on riemetto il sfl tal quale,
055500011127      *                             pulendo solo l'ultima opzione scelta
055600011127      *              se ho F06 =*on ricarico il sfl
055700011127      *
055800011203      * Se provengo dalla prima videata, o la ricarico, o se ho F03
055900011203      * attivo esco definitivamente
056000011210 2a  C     TipVid        ifeq      '1'
056100011210 3a  C     O31Tfu        ifeq      'F03'
056200011203     C                   eval      $Fine = 'S'
056300011203     C                   else
056400011109     C                   eval      $CarD1 ='1'
056500011210 3-  C                   endif
056600011210 2-  C                   endif
056700011120      *
056800011210 2a  C     TipVid        ifeq      '2'
056900011127      *
057000011210 3a  C     O31Tfu        ifeq      'F03'
057100011128      *
057200011128      * Segnalo che devo poi uscire dal loop delle readc su sfl
057300011128      * per riemetterlo tal quale e non richiamare il Tibs31R
057400011220     C                   eval      $leave10 = *on
057500011220     C                   eval      $Carsfl  ='0'
057600011210 3-  C                   endif
057700011127      *
057800011210 3a  C     O31Tfu        ifeq      'F06'
057900011120     C                   eval      $Carsfl='1'
058000011210 3-  C                   endif
058100011210 2-  C                   endif
058200011210      *
058300011109     C                   ENDSR
058400011109      *--------------------------------------------------------------------------------------------*
058500011109      * CarD1  - Carica primo video
058600011109      *--------------------------------------------------------------------------------------------*
058700011109     C     CarD1         BEGSR
058800011109      *
058900011120     c                   setoff                                       28
059000011112     c                   setoff                                       414243
059100130704     c                   setoff                                       44
059200011112     C                   clear                   V1CUte
059300011112     C                   clear                   V1CFil
059400011112     C                   clear                   V1CFun
059500011112     C                   clear                   V1CAut
059600110110     C                   clear                   V1CTxt
059700130704     C                   clear                   V1Cmopr
059800011109      *
059900011109     C                   ENDSR
060000011109      ************************************************************
060100011109      * Controlla e decodifica la prima videata
060200011109      ************************************************************
060300011109     C     CtrD1         BEGSR
060400011109      *
060500011120     C                   setoff                                       28
060600011109     C                   setoff                                       414243
060700130704     C                   setoff                                       44
060800011109     C                   clear                   V1Msg
060900011109      *
061000011112      * Controllo Filiale
061100011109     C     V1CFil        ifne      *zeros
061200011112     c                   move      V1CFil        kFil              3 0
061300011203     c     kFil          chain     AzOrg01l                           30
061400011120     C   30              seton                                        2841
061500011120     C   30              eval      V1Msg = Msg(1)
061600011112     C                   endif
061700011109      *
061800011109      * Controllo Funzione
061900011113     C     V1Cfun        ifne      *blanks
062000011109     C                   clear                   tibs02ds
062100011109     C                   movel     'C'           t02mod
062200011109     C                   movel     *blanks       t02sif
062300011109     C                   movel     'FUN'         t02cod
062400011109     C                   movel     V1CFun        t02ke1
062500011109     C                   call      'TIBS02R'
062600011109     C                   parm                    KPJBA
062700011109     C                   parm                    TIBS02DS
062800011109     C                   if        t02err <> *blanks
062900011120     C                   seton                                        2842
063000011112     C                   eval      V1Msg = Msg(2)
063100011109     C                   endif
063200011113     C                   endif
063300011109      *
063400011112      * Controllo Livello autorità
063500011113     C     V1CAut        ifne      *blanks
063600011109     C                   clear                   tibs02ds
063700011109     C                   movel     'C'           t02mod
063800011109     C                   movel     *blanks       t02sif
063900011109     C                   movel     'LAT'         t02cod
064000011109     C                   movel     V1CAut        t02ke1
064100011109     C                   call      'TIBS02R'
064200011109     C                   parm                    KPJBA
064300011109     C                   parm                    TIBS02DS
064400011109     C                   if        t02err <> *blanks
064500011120     C                   seton                                        2843
064600011109     C                   eval      V1Msg = Msg(3)
064700011109     C                   endif
064800011113     C                   endif
064900130704      *
065000130704      * Controllo Tipo Modulo
065100130704     C     V1Cmopr       ifne      *blanks
065200130704     C                   clear                   tibs02ds
065300130704     C                   movel     'C'           t02mod
065400130704     C                   movel     *blanks       t02sif
065500130704     C                   movel     'MDP'         t02cod
065600130704     C                   movel     V1Cmopr       t02ke1
065700130704     C                   call      'TIBS02R'
065800130704     C                   parm                    KPJBA
065900130704     C                   parm                    TIBS02DS
066000130704     C                   if        t02err <> *blanks
066100130704     C                   seton                                        2844
066200130704     C                   eval      V1Msg = Msg(6)
066300130704     C                   endif
066400130704     C                   endif
066500011109      *
066600011109     C                   ENDSR
066700011109      *--------------------------------------------------------------------------------------------*
066800011109      * GesS1  - Gestione video subfile
066900011109      *--------------------------------------------------------------------------------------------*
067000011109     C     GesS1         BegSr
067100011120     C                   setoff                                       24
067200011107      *
067300011109      * Inizializzo la videata
067400011210 1a  C     $CarSfl       ifeq      *on
067500011109     C                   exsr      CarSfl
067600011210 1-  C                   endif
067700011107      *
067800011109      * Scrivo il piede
067900011113     C                   write     Bs30P1
068000011109      * Se non ci sono dati da caricare e non ho richesto il posizionamento
068100011210 1a  C     wmax          ifeq      0
068200011112     c                   setoff                                       20
068300011120     c                   seton                                        2124
068400011113     C                   write     Bs30D2                                       NO RCD
068500011210 1-  C                   endif
068600011109      *
068700011109      * Emetto il control
068800011113     C                   exfmt     Bs30C1
068900011114      *
069000011120     C     '?'           Scan      VCCFun                                 47
069100011120     C     '?'           Scan      VCCAut                                 48
069200130704     C     '?'           Scan      VCCmopr                                50
069300011109      *
069400011210 1a  C                   SELECT
069500011109      *
069600011109      * F03=Fine
069700011109     C     *inkc         Wheneq    *on
069800011109     C                   EXSR      F03Ges
069900011114      *
070000011114     C     *in47         wheneq    *on
070100011114     C     *in48         oreq      *on
070200130704     C     *in50         oreq      *on
070300011114     C                   EXSR      Search
070400130704
070500130704      * F8=Ordinamento x cod.utente
070600130704     c                   WHEN      *inKH
070700130704     c                   eval      wFIL = VCCfil
070800130704     c                   EXSR      F08Ges
070900130704     c   28              seton                                        41
071000130704     c   28              eval      C1Msg = Msg(7)
071100011210      *
071200011210      * F10=Immissione
071300011210     C     *inkj         wheneq    *on
071400011210     C                   EXSR      F10Ges
071500011220      *
071600011220      * Eseguo il controllo opzioni solo se non sono
071700020108      * uscita con F03 da F10Ges e se ho dei record caricati
071800011220     C     $leave10      ifeq      *off
071900020108     C     WMax          andgt     0
072000011220     C                   EXSR      CtrS1
072100011220     C                   endif
072200011210      *
072300011210 1e  C                   OTHER
072400011112      *
072500011113      * Controllo dati e posizionamento
072600011109     C                   EXSR      CtrC1
072700011109      *
072800011120     C     *in28         ifeq      *off
072900020108     C     WMax          andgt     0
073000011220      *
073100020108      * Controllo le opzioni solo se non è stato richiesto il posizionamento
073200011220     C     $Pos          ifne      *on
073300011112     C                   EXSR      CtrS1
073400011220     C                   else
073500011220     C                   move      *off          $Pos
073600011220     C                   endif
073700011220     C                   endif
073800011109      *
073900011210 1-  C                   ENDSL
074000011109      *
074100011109     C                   ENDSR
074200011109      *--------------------------------------------------------------------------------------------*
074300011109      * Carsfl - Caricamento sfile
074400011109      *--------------------------------------------------------------------------------------------*
074500011109     C     CarSfl        BEGSR
074600011109      *
074700011126     C                   setoff                                           30
074800011109     C                   z-add     *zeros        RecSf                          *n° record sfile
074900011112     C                   movel     '2'           TipVid                         *gestione 1° video
075000011109      * Pulizia sfile
075100011112     C                   setoff                                       2021      *non visualizza sfl
075200011113     C                   write     Bs30C1
075300011112     C                   seton                                        2021
075400130704      * Prima di caricare il subfile devo caricare la SK dei profili standard
075500130704     c                   exsr      ABISTD
075600011109      *
075700011109      * Caricamento sfile -tutto-
075800011109     C                   EXSR      ScrSfl                                       *scrive sfile
075900011203      *
076000011203      * Posizionamento cursore su sfl
076100011203     C                   SELECT
076200011203      *
076300011203      * Se ho richesto rollup ma sono a fine lettura sfl,
076400011203      * continuo a posizionarmi sull'ultimo record del sfl
076500011203     C     *in25         wheneq    *on
076600011203     C     *in23         andeq     *on
076700011203     C                   z-add     wmax          $RecSf                         *posiziona in fondo
076800011203      *
076900011203      * Se ho riempito almeno una riga di sfl, e se non provengo
077000011203      * da F06 del tibs31R, mi riposiziono sul primo record.
077100011203     C     $RecSf        wheneq    0
077200011203     C     RecSf         orne      0
077300011203     C     O31Tfu        andne     'F06'
077400011203     C                   z-add     1             $RecSf                         *posiziona in fondo
077500120402      *
077600120402      * Se ho riempito almeno una riga di sfl, e se provengo
077700120402      * da F06 del tibs31R, mi riposiziono sul record in cui
077800120402      * ero  o  sull'ultimo della pagina
077900120402     C     RecSf         whenne    0
078000120402     C     O31Tfu        andeq     'F06'
078100120402     C     $RecSf        andgt     wMax
078200120402     C                   z-add     wMax          $RecSf                         *posiziona in fondo
078300120402      *
078400011203     C                   ENDSL
078500011109      *
078600011203     C                   move      '0'           $carsfl
078700011220     C                   movel     *blanks       O31Tfu
078800011203      *
078900011107     C                   ENDSR
079000011109      *--------------------------------------------------------------------------------------------*
079100011109      * ScrSfl - Scrive il sfile -tutto-
079200011109      *--------------------------------------------------------------------------------------------*
079300011109     C     ScrSfl        BEGSR
079400100409      *
079500100409      * Reperimento data odierna
079600100409     c                   eval      wDate_Iso = %date()
079700100409     c                   eval      wDate     = %dec(wDate_Iso)
079800011109      *
079900011112      * Imposta dati
080000011107     C                   z-add     0             wmax
080100011112     C                   move      *blanks       $Mod              1
080200011112     C                   move      '1'           $OkSel            1
080300011112     C                   movel     *all'9'       All9             10
080400011112      *
080500011120      * Se i campi di work sono valorizzati utilizzo questi per i confronti
080600011120      * Sposto quindi i campi da utilizzare per le selezioni in altri campi.
080700011120      * Se il profilo non è valorizzato e la filiale è diversa da zeros
080800011120      * allora imposto $Mod='2', altrimenti imposto $Mod='1'.
080900100409      * Sia per $Mod='1' che per $Mod='2' utilizzo AzUte01l.
081000011120      * Eseguo il check di validità per autorità e finzione, se $mod='1'
081100011120      * anche per filiale.
081200011203 1a  C     W1Ute         ifne      *blanks
081300011120     C                   eval      W2Ute = W1Ute
081400011203 1e  C                   else
081500011120     C                   eval      W2Ute = VCCUte
081600011203 1-  C                   endif
081700011120      *
081800011203 1a  C     W1Fun         ifne      *blanks
081900011120     C                   eval      W2Fun = W1Fun
082000011203 1e  C                   else
082100011120     C                   eval      W2Fun = VCCFun
082200011203 1-  C                   endif
082300011120      *
082400011203 1a  C     W1Aut         ifne      *blanks
082500011120     C                   eval      W2Aut = W1Aut
082600011203 1e  C                   else
082700011120     C                   eval      W2Aut = VCCAut
082800011203 1-  C                   endif
082900110105      *
083000110105 1a  C     W1txt         ifne      *blanks
083100110105     C                   eval      W2txt = W1txt
083200110105 1e  C                   else
083300110105     C                   eval      W2txt = VCCtxt
083400110105 1-  C                   endif
083500011120      *
083600011203 1a  C     W1Fil         ifne      *zeros
083700011120     C                   move      W1Fil         W2Fil
083800011203 1e  C                   else
083900011120     C                   move      VCCFil        W2Fil
084000011203 1-  C                   endif
084100130704      *
084200130704     c                   IF        w1mopr <> *blanks
084300130704     c                   eval      w2mopr = w1mopr
084400130704     c                   ELSE
084500130704     c                   eval      w2mopr = VCCmopr
084600130704     c                   ENDIF
084700011120      *
084800011203 1a  C     W2Ute         ifeq      *blanks
084900011120     C     W2Fil         andne     *zeros
085000011120     C                   eval      $Mod = '2'
085100011203 1e  C                   else
085200011120     C                   eval      $Mod = '1'
085300011203 1-  C                   endif
085400011112      *
085500011112      * Creo limite massimo delle parzializzazioni
085600011120     C     W2Ute         Cat       ALL9:0        WUte999          10
085700011120      *
085800011203 1a  C     $Mod          ifeq      '1'
085900011120     C     W2Ute         setll     AzUte01l
086000011120     C                   read      AzUte01l                               30
086100011112      *
086200011203 1e  C                   else
086300100409     C     *loval        setll     AzUte01l
086400100409     C                   read      AzUte01l                               30
086500011203 1-  C                   endif
086600011112      *
086700011112      * Ciclo di lettura sulla tabella da caricare
086800011203 1a  C                   dow       not *in30
086900011120     C                   move      '1'           $OkSel            1
087000130704     c                   eval      dUte02 = UTEfa1
087100130930     c                   eval      dUte01 = UTEfaf
087200011112      *
087300011112      * Controllo, se sono in modalità 1, il limite max del cod.profilo
087400011203 2a  C     $Mod          ifeq      '1'
087500011120     C     UteUte        andlt     W2Ute
087600011120     C     UteUte        andgt     WUte999
087700011112     C                   move      '0'           $OkSel
087800011203 2-  C                   endif
087900011112      *
088000011112      * Controllo Funzione e autorizzazione
088100011203 2a  C     W2Fun         ifne      *blanks
088200011120     C     W2Fun         andne     UteFun
088300011120     C     W2Aut         orne      *blanks
088400011120     C     W2Aut         andne     UteAut
088500100409     C     W2Fil         orne      *zeros
088600011120     C     W2Fil         andne     UteFil
088700130704     C     w2mopr        orne      *blanks
088800130704     C     w2mopr        andne     §UT2mopr
088900011120     C                   move      '0'           $OkSel
089000011203 2-  C                   endif
089100110105      * Controllo descrizione (testo)
089200110105 2a  C     W2txt         ifne      *blanks
089300110105     c     uteute        chain     kpprf01l
089400110105     c                   if        not %found(kpprf01l)
089500110105     C                   move      '0'           $OkSel
089600110105     c                   else
089700110110     c     lo:up         xlate     prftxt        wprftxt
089800110110     C                   EVAL      %len(wrk2txt) = %len(%trim(w2txt))
089900110110     c                   eval      wrk2txt = %trim(w2txt)
090000110110     c     wrk2txt       scan      wprftxt
090100110110     c                   if        not %found
090200110110     C                   move      '0'           $OkSel
090300110110     c                   endif
090400110110     c
090500110105     c                   endif
090600110105     c                   endif
090700011112      *
090800011203 2a  C     $OkSel        ifeq      '1'
090900011107      *
091000011109      * Riempimento campi subfile
091100011112     C                   exsr      RieS1
091200011112      *
091300011109      * Scrivo sfile
091400011109     C                   add       1             RecSf                          *incrementa n°rec
091500130704     c                   IF        *in38
091600130704     c                   eval      redute = '1'
091700130704     c                   ENDIF
091800140710     c                   IF        *in51
091900140709     c                   eval      bluute = '1'
092000140709     c                   ENDIF
092100011107      *
092200011203     C                   write     Bs30S1                                       *scrive sfile
092300011112     C                   z-add     RecSf         wmax                           *n° rel sf x stampa
092400011203 2-  C                   endif
092500011107      *
092600011112      * Continuo a leggere a seconda della modalità
092700011203 2a  C     $Mod          ifeq      '1'
092800011120     C                   read      AzUte01l                               30
092900011203 2-  C                   endif
093000011203 2a  C     $Mod          ifeq      '2'
093100100409     C                   read      AzUte01l                               30
093200011203 2-  C                   endif
093300011203 1-  C                   ENDDO
093400011112      *
093500011211     C                   movel     VCCUte        w1Ute
093600011211     C                   move      VCCFil        w1Fil
093700011211     C                   move      VCCFun        w1Fun
093800011211    >C                   move      VCCAut        w1Aut
093900110110    >C                   move      VCCTxt        w1txt
094000130704     c                   eval      w1mopr = VCCmopr
094100011109      *
094200011107     C                   ENDSR
094300011112      *--------------------------------------------------------------------------------------------*
094400040729      * Richiamo pgm. TIBS31R nelle diverse modalità
094500011112      *--------------------------------------------------------------------------------------------*
094600040729     C     CallTIBS31    BEGSR
094700011107      *
094800011112      * valorizzo profilo e opzione
094900011203     C                   clear                   Tibs31Ds
095000130705     C                   eval      I31Ute = wUTE
095100011203     C                   eval      I31Opz = VSCOpz
095200130705     c                   eval      I31fil = %editc(wFIL:'X')
095300140710     c                   IF        HIDfun = 'COCOM' or
095400140710     c                             HIDfun = 'REA' or
095500140710     c                             HIDfun = 'RED'
095600130717     c                   eval      I31fun = HIDfun
095700130717     c                   ENDIF
095800011107      *
095900011107      * Richiamo pgm di gestione
096000160822     c                   If        TipVid <> '3'
096100011109     C                   call      'TIBS31R'
096200011107     C                   PARM                    kpjba
096300011203     C                   PARM                    Tibs31Ds
096400160822     c                   Else
096500160822     c                   call      'TIBS31R'
096600160822     c                   parm                    KPJBA
096700160822     c                   parm                    TIBS31ds
096800160822     c                   parm                    TIBS31ds2
096900160822     c                   EndIf
097000011107      *
097100011127      * Distinguo i casi con cui sono uscito da tibs31r:
097200011127      *              se ho F03 =*on riemetto il sfl tal quale,
097300011127      *                             pulendo solo l'ultima opzione scelta
097400011127      *              se ho F06 =*on ricarico il sfl
097500011127      *
097600011210 1A  C     O31Err        IFEQ      *blanks
097700011127      *
097800011210 2a  C     O31Tfu        ifeq      'F03'
097900011128      *
098000011128      * Segnalo che devo poi uscire dal loop delle readc su sfl
098100011128      * per riemetterlo tal quale e non richiamare il Tibs31R
098200011128      * se presenti eventuali altre opzioni.
098300011127     C                   eval      $leave  = *on
098400011127     C                   eval      $Carsfl ='0'
098500011210 2-  C                   endif
098600011127      *
098700011210 2a  C     O31Tfu        ifeq      'F06'
098800011127     C                   eval      $Carsfl='1'
098900011210 2-  C                   endif
099000011210      *
099100011210      * C'è un errore nel richiamo di tibs31r
099200011210      * emetto il meggaggio.
099300011210 1E  C                   ELSE
099400011210      *
099500011210     C                   clear                   C1Msg
099600011210     C                   seton                                        28
099700011210     C                   eval      C1Msg = O31Msg
099800011210 1-  C                   endif
099900011127      *
100000011107     C                   ENDSR
100100040729      *-----------------------------------------------------------
100200040729      * Richiamo pgm. TIBS32R
100300040729      *-----------------------------------------------------------
100400040729     c     CallTIBS32    BEGSR
100500040729      *
100600040729     c                   clear                   TIBS32ds
100700040729     c                   select
100800080912     c                   when      VSCopz = '1'
100900080912     c                   eval      I32opz = '7'
101000081030     c                   when      VSCopz = '4'
101100081030     c                   eval      I32opz = '8'
101200040729     c                   when      VSCopz = '7'
101300040729     c                   eval      I32opz = '5'
101400040729     c                   when      VSCopz = '8'
101500040729     c                   eval      I32opz = '2'
101600040729     c                   endsl
101700040729     c                   eval      I32ute = VSCute
101800040729     c                   movel(p)  TIBS32ds      KPJBU
101900040729      *
102000040729     c                   call      'TIBS32R'
102100040729     c                   parm                    KPJBA
102200040729      *
102300040729     c                   movel     KPJBU         TIBS32ds
102400040729     c                   if        O32err = 'E'
102500040729     c                   clear                   $Fine
102600040729     c                   seton                                        28  40
102700040729     c                   movel     O32msg        C1msg
102800040729     c                   endif
102900040729      *
103000040729     c                   ENDSR
103100100407      *-----------------------------------------------------------
103200100407      * Richiamo pgm. TNTBAUTR
103300100407      *-----------------------------------------------------------
103400130930     c*m   CallTBAUTR    BEGSR
103500100407      *
103600130930     c*m                 clear                   TNTBautDS
103700130930     c*m                 eval      autSELEZ = 'R'
103800130930     c*m                 move(p)   vscFIL        autFILIA
103900130930     c*m                 eval      autPROF  = vscUTE
104000130930     c*m                 movel(p)  TNTBautDS     KPJBU
104100100407      *
104200130930     c*m                 call      'TNTBAUTR'
104300130930     c*m                 parm                    KPJBA
104400130930     c*m                 movel     KPJBU         TNTBautDS
104500130930      *m
104600130930     c*m                 ENDSR
104700011109      *--------------------------------------------------------------------------------------------*
104800011109      * CtrS1  - Controlla campi sfile
104900011109      *--------------------------------------------------------------------------------------------*
105000011109     C     Ctrs1         BEGSR
105100011109      *
105200011210     C                   setoff                                       222990
105300130709     c                   clear                   wFIL
105400011107      *
105500011109      * Lettura dei record "toccati" del sfile
105600011109     C                   z-add     1             RecSf
105700011203     C                   readc     Bs30s1                                 29
105800011220     C                   dow       not *in29
105900011107      *
106000011109      * Gestione opzioni
106100011107      *
106200011107      * Opzioni
106300040729     C                   if        VSCopz <> ' '
106400011107      *
106500040729     c                   select
106600130704     c                   when      knmus <> 'EDPAC' and
106700130704     c                             knmus <> 'EDPND' and
106800130704     c                             knmus <> 'EDPMB' and
106900130705     c                             knmus <> 'EDPGA' and
107000130705     c                             knmus <> 'EDPLB' and
107100130704     c                             VSCmopr = 'S' and
107200130704     c                             VSCopz <> '3' and
107300130704     c                             VSCopz <> '5' and
107400130704     c                             VSCopz <> '7'
107500130704     c                   seton                                        2840
107600130704     C                   eval      C1Msg = Msg(4)
107700130723      * Copia da profilo no STD no 046 (tolgo questo mess fino a quando non ci sono profili STD)
107800130808     c******             when      knmus <> 'EDPAC' and
107900130808     c******                       knmus <> 'EDPND' and
108000130808     c******                       knmus <> 'EDPMB' and
108100130808     c******                       knmus <> 'EDPGA' and
108200130808     c******                       knmus <> 'EDPLB' and
108300130808     c******                       VSCmopr <> 'S' and
108400130808     c******                       VSCfil <> 046 and
108500130808     c******                       VSCopz = '3'
108600130723      * (tolgo questa messaggio fino a quando non ci sono profili STD)
108700130723     c******             seton                                        2840
108800130723     C******             eval      C1Msg = Msg(4)
108900130705      * Copia da profilo STD
109000130705     c                   when      VSCopz = '3' and VSCmopr = 'S'
109100160819     c*//                exsr      gesw03
109200160819     c                   exsr      sr_GesWO3
109300040729      * Gestione richiamo TIBS31R
109400100114     C                   when      VSCopz = '2' or VSCopz = '3' or
109500100114     C                             VSCopz = '5' or VSCopz = '9'
109600130705     c                   eval      wUTE = VSCute
109700040729     c                   exsr      CallTIBS31
109800140121      *   Controlla se è stata annullata la tabella quindi pulisce il flag
109900140121      *      di Profilo presente sulla tabella "AUT"
110000140121     c                   clear                   vscTBaut
110100140121     c                   if        §UTEAUTOR <> 'NO' and §UTEAUTOR <> *blank
110200140121     c                   move      'S'           vscTBaut
110300140121     c                   end
110400040729      * Gestione richiamo TIBS32R
110500080912     C                   when      VSCopz = '7' or VSCopz = '8' or
110600081030     C                             VSCopz = '1' or VSCopz = '4'
110700040729     c                   exsr      CallTIBS32
110800100407      * Gestione richiamo TNTBAUTR
110900130930     C*m                 when      VSCopz = 'A'
111000130930     c*m                 exsr      CallTBAUTR
111100120926      *
111200120926      *
111300040729     c                   endsl
111400011109      *
111500011109      * Aggiorna sfile
111600011109     c                   EXSR      AggSfl                                       *aggiorna sfile
111700011107     c                   endif
111800011128      *
111900011128      * testo il flag per la richiesta forzata di fine lettura readc
112000011128      * su sfl, poichè provengo da richiesta F03 del tibs31r
112100011128      * se presenti eventuali altre opzioni.
112200011128     C     $leave        ifeq      *on
112300011220     C                   move      *off          $leave            1
112400011128     C                   leave
112500011128     C                   endif
112600011109      *
112700011109      * lettura successiva record
112800011203     C                   readc     Bs30s1                                 29
112900011107     C                   enddo                                                  *fine sfile
113000011109      *
113100011107     C                   ENDSR
113200011107      ************************************************************
113300011113      * Controllo testata e posizionamento
113400011107      ************************************************************
113500011109     C     Ctrc1         BEGSR
113600011112      *
113700011113     C                   clear                   C1Msg
113800011130     C                   setoff                                       2823
113900011113     C                   setoff                                       414243
114000011130      * Controllo Rollup
114100011130     C     *in25         ifeq      *on
114200011130     C                   seton                                        2823
114300011130     C                   eval      C1Msg = Msg(5)
114400011130     C                   eval      $carSfl = *on
114500011130     C                   endif
114600011113      *
114700011113      * Controllo Filiale
114800011120     C     VCCFil        ifne      *zeros
114900011120     c                   move      VCCFil        kFil              3 0
115000011203     c     kFil          chain     AzOrg01l                           30
115100011120     C   30              seton                                        2841
115200011120     C   30              eval      C1Msg = Msg(1)
115300011113     C                   endif
115400011113      *
115500011113      * Controllo Funzione
115600011120     C     VCCFun        ifne      *blanks
115700011113     C                   clear                   tibs02ds
115800011113     C                   movel     'C'           t02mod
115900011113     C                   movel     *blanks       t02sif
116000011113     C                   movel     'FUN'         t02cod
116100011120     C                   movel     VCCFun        t02ke1
116200011113     C                   call      'TIBS02R'
116300011113     C                   parm                    KPJBA
116400011113     C                   parm                    TIBS02DS
116500011113     C                   if        t02err <> *blanks
116600011120     C                   seton                                        2842
116700011120     C                   eval      C1Msg = Msg(2)
116800011113     C                   endif
116900011113     C                   endif
117000011113      *
117100011113      * Controllo Livello autorità
117200011120     C     VCCAut        ifne      *blanks
117300011113     C                   clear                   tibs02ds
117400011113     C                   movel     'C'           t02mod
117500011113     C                   movel     *blanks       t02sif
117600011113     C                   movel     'LAT'         t02cod
117700011120     C                   movel     VCCAut        t02ke1
117800011113     C                   call      'TIBS02R'
117900011113     C                   parm                    KPJBA
118000011113     C                   parm                    TIBS02DS
118100011113     C                   if        t02err <> *blanks
118200011120     C                   seton                                        2843
118300011120     C                   eval      C1Msg = Msg(3)
118400011113     C                   endif
118500011113     C                   endif
118600130704      *
118700130704      * Controllo Tipo Modulo
118800130704     c                   IF        VCCmopr <> *blanks
118900130704     c                   clear                   tibs02ds
119000130704     c                   movel     'C'           t02mod
119100130704     c                   movel     *blanks       t02sif
119200130704     c                   movel     'MDP'         t02cod
119300130704     c                   movel     VCCmopr       t02ke1
119400130704     C                   call      'TIBS02R'
119500130704     C                   parm                    KPJBA
119600130704     C                   parm                    TIBS02DS
119700130704     C                   if        t02err <> *blanks
119800130704     C                   seton                                        2844
119900130704     C                   eval      C1Msg = Msg(6)
120000130704     C                   endif
120100130704     C                   endif
120200011203      *
120300011120      * Per riposizionarmi non ci devono essere errori
120400011120      * devo avere record da caricare e non devo essere nel caso
120500011120      * di NO REC.
120600011120     C     *in28         ifeq      *off
120700011112      *
120800011109      *
120900011120     C     VCCUte        ifne      w1Ute
121000011120     C     VCCFil        orne      w1Fil
121100011120     C     VCCFun        orne      w1Fun
121200011120     C     VCCAut        orne      w1Aut
121300110110     C     VCCTxt        orne      w1txt
121400130704     C     VCCmopr       orne      w1mopr
121500011107      *
121600011112     C                   EXSR      Poss1
121700011107     C                   endif
121800011120     C                   endif
121900011109      *
122000011107     C                   ENDSR
122100011107      ************************************************************
122200011107      *  Posizionamento su lista
122300011107      ************************************************************
122400011112     C     Poss1         BEGSR
122500011109      *
122600011109      * Controllo se esistono opzioni attive
122700011128     C                   setoff                                       90
122800011220      *
122900011220      * Segnalo il posizionamento
123000011220     C                   move      *on           $Pos              1
123100011109      *
123200011107     C     wmax          ifgt      0
123300011203     C                   readc     Bs30s1                                 29
123400011120     C     *in29         doweq     *off
123500011126     C     *in90         andeq     *off
123600011109      *
123700011120     C     VSCOpz        ifne      ' '
123800011126     C                   seton                                        90
123900011112     C                   endif
124000011109      * Se rilevata opzione attiva, attivo sflnxtchg
124100011126     C     *in90         ifeq      *on
124200100409     c                   eval      *in39 = (HIDdsc < wDate)
124300011120     C                   move      *on           *in22
124400130704      * reimposto l'utente in rosso
124500130704     c                   IF        redute = '1'
124600130704     c                   eval      *in38 = *on
124700130704     c                   ELSE
124800130704     c                   eval      *in38 = *off
124900130704     c                   ENDIF
125000140709      * reimposto l'utente in blu
125100140709     c                   IF        bluute = '1'
125200140710     c                   eval      *in51 = *on
125300140709     c                   ELSE
125400140710     c                   eval      *in51 = *off
125500140709     c                   ENDIF
125600011107      *
125700011203     C                   update    Bs30s1
125800011112     C                   else
125900011203     C                   readc     Bs30s1                                 29
126000011112     C                   endif
126100011109      *
126200011112     C                   enddo
126300011109      *
126400011112     C                   endif
126500011109      *
126600011126     C     *in90         ifeq      *off
126700011120     C                   movel     VCCUte        w1Ute
126800011120     C                   move      VCCFil        w1Fil
126900011120     C                   move      VCCFun        w1Fun
127000011120    >C                   move      VCCAut        w1Aut
127100110110    >C                   move      VCCTxt        w1Txt
127200130704     c                   eval      w1mopr = VCCmopr
127300011109      *
127400011109      * Richiedo l'inizializzazione sfl
127500011107     C                   move      *on           $carsfl
127600011109      *
127700011126     C                   endif
127800011109      *
127900011107     C                   ENDSR
128000011109      *--------------------------------------------------------------------------------------------*
128100011109      * AggSfl - Aggiorna riga di sfile
128200011109      *--------------------------------------------------------------------------------------------*
128300011109     C     Aggsfl        BEGSR
128400100409      *
128500100409      * Reimpostazione attributi di visualizzazione
128600100409     c                   eval      *in39 = (HIDdsc < wDate)
128700100409      *
128800011107      * aggiorna sfile
128900011120     c                   Z-ADD     RecSf         $RecSf                         *posizionamento
129000011120     c                   SETON                                        22
129100011120     c                   clear                   VSCOpz
129200130704      * reimposto l'utente in rosso
129300130704     c                   IF        redute = '1'
129400130704     c                   eval      *in38 = *on
129500130704     c                   ELSE
129600130704     c                   eval      *in38 = *off
129700130704     c                   ENDIF
129800140709      * reimposto l'utente in blu
129900140709     c                   IF        bluute = '1'
130000140710     c                   eval      *in51 = *on
130100140709     c                   ELSE
130200140710     c                   eval      *in51 = *off
130300140709     c                   ENDIF
130400011107      *
130500011203     C                   update    Bs30s1
130600011128     c                   setoff                                       22
130700011109      *
130800011107     C                   ENDSR
130900011109      *--------------------------------------------------------------------------------------------*
131000011109      * RieS1  - Valorizzazione dei campi del subfile
131100011109      *--------------------------------------------------------------------------------------------*
131200011109     C     Ries1         BEGSR
131300120402      *
131400120402     c                   clear                   BS30S1
131500100114      *
131600100114      * - Impostazione flag (NON di abilitazioni)
131700100114     c                   eval      dUte02 = UTEfa1
131800130930     c                   eval      dUte01 = UTEfaf
131900011109      *
132000011112      * Profilo
132100011120     C                   eval      VSCUte  =  UteUte
132200130703
132300011112      * Data scadenza
132400100409     c                   eval      HIDdsc  =  UTEdsc
132500011120     C                   reset                   WLBDA8
132600020329     C                   Z-ADD     Utedsc        G08INV
132700011120     C                   Z-ADD     *zeros        G08DAT
132800011120     C                   move      '3'           G08ERR
132900011120     C                   CALL      'XSRDA8'
133000011120     C                   PARM                    WLBDA8
133100011120      *
133200011120     C     G08err        ifeq      '0'
133300020329     C                   move      G08Dat        VSCdsc
133400011120     C                   endif
133500011112      * P.O.
133600011120     C                   move      UteFil        VSCFil
133700100416      *
133800100416      * flag di Profilo presente sulla tabella "AUT"
133900130930     c                   clear                   vscTBaut
134000130930     c                   if        §UTEAUTOR <> 'NO' and §UTEAUTOR <> *blank
134100130930     c                   move      'S'           vscTBaut
134200130930     c                   end
134300100416      *
134400011112      * Codice utente
134500020329     C                   move      UteCdu        VSCCdu
134600011112      * Funzione
134700130709     c                   eval      hidfun = UTEfun
134800060512     c* sostituito con s.informativo per sapere se si tratta di profilo che
134900060512     c*  si collega in sede o filiale
135000060512     c     uteute        chain     kpprf01l
135100060512     c                   if        %found(kpprf01l)
135200060512     c                   eval      vscsif=prfnsi
135300110111      * Descrizione
135400110111     c                   eval      vsctxt  =  prftxt
135500060512     c                   else
135600060512     c                   eval      vscsif='NoPresente'
135700110111      * Descrizione
135800110111     c                   clear                   vsctxt
135900060512     c                   endif
136000100114      * Modulo Profilo
136100100114     c                   eval      VSCmopr = §UT2mopr
136200100409      *
136300120213     C                   EVAL      vscProj = GetProfiloUtenteProj(vscUte)
136400100409      * Impostazione attributi di visualizzazione
136500100409     c                   eval      *in39 = (HIDdsc < wDate)
136600140528
136700140528      /free
136800140528       *in38 = *off;
136900140710       *in51 = *off;
137000140528       clear xx;
137100140528
137200140528       // se non è un utente STANDARD verifico se ha differenze con il suo STD
137300140528       // i controlli NON li faccio se utente Scaduto
137400140709       // particolarità di UTENTE STD con funzione aziendale COCOM - RED - REA
137500140528       // particolarità di UTENTE STD per filiali messaggerie
137600140528       IF  §UT2mopr <> 'S' and not *in39;
137700140528
137800140528         // se funzione NO COCOM prendo i primi 3 dell'utente per cercare il suo STD
137900140709         // stessa cosa per funzione NO RED e REA
138000140709         IF  UTEfun <> 'COCOM' and UTEfun <> 'RED' and UTEfun <> 'REA';
138100140528
138200140528           // se non è filiale utente messaggeria di area devo aggiungere una 'M'
138300140528           // al profilo per cercare il profilo STD delle messaggerie standard
138400140528           wrkUTE = %subst(VSCute:1:3);
138500140528           IF  IsFilMess;
138600140528             wrkUTE = %trim(wrkUTE) + %trim('M');
138700140528           ENDIF;
138800140528           xx = %lookup(wrkUTE:skUTE);
138900140528         ELSE;
139000140528         // se funzione COCOM devo cercare lo STD COCOM
139100140709           IF  UTEfun = 'COCOM';
139200140528           xx = %lookup('COCOM':skUTE);
139300140710           *in51 = *on;
139400140709           ENDIF;
139500140710         // se funzione RED devo cercare lo STD RDRED
139600140709           IF  UTEfun = 'RED';
139700140710             xx = %lookup('RDRED':skUTE);
139800140710             *in51 = *on;
139900140709           ENDIF;
140000140710         // se funzione REA devo cercare lo STD RAREA
140100140710           IF  UTEfun = 'REA';
140200140710             xx = %lookup('RAREA':skUTE);
140300140710             *in51 = *on;
140400140710           ENDIF;
140500140528         ENDIF;
140600140528         // Devo controllare il campo UTEFAF (DUTE01) senza i campi
140700140528         // §UTEAUTOR e §UTEDSAUT (UTEAUTDS)
140800140528         IF  xx > 0 and
140900140528            (%subst(skABI(xx):1:58) <> %subst(UTEfaf:1:58) or
141000140528             %subst(skABI(xx):96:%len(UTEfaf)-96+1) <>
141100140528             %subst(UTEfaf:96:%len(UTEfaf)-96+1) or
141200140528             skFUN(xx) <> UTEfun or
141300140528             skAUT(xx) <> UTEaut);
141400140528           *in38 = *on;
141500140528         ENDIF;
141600140528         // se utente con S.I. GAITRA201 e i primi 3 byte = allo STD
141700140528         // non devo evidenziare le differenze
141800140528         IF  *in38 and VSCsif = 'GAITRA201 ' and
141900140528             %subst(skute(xx):1:3) = %subst(VSCute:1:3);
142000140528           *in38 = *off;
142100140528         ENDIF;
142200140528       ENDIF;
142300140528      /end-free
142400140528
142500011107     C                   ENDSR
142600011109      *--------------------------------------------------------------------------------------------*
142700011120      *   Routine iniziale
142800011109      *--------------------------------------------------------------------------------------------*
142900011122     C     InzVar        BEGSR
143000011109      *
143100011109      * Ricevimento parametri
143200011107     C     *ENTRY        PLIST
143300011108     C                   PARM                    kpjba
143400020820      *
143500100416     C     Tbe01L        Klist
143600100416     c                   Kfld                    TbeCOD
143700100416     c                   Kfld                    TbeKE1
143800100416      *
143900020820      * Reperisco dati job
144000020820     C                   EXSR      DatiJob
144100130710
144200130710      * Controllo se richiamato
144300130710     c                   IF        %subst(kpjbu:1:3) <> *blanks and
144400130710     c                             %subst(kpjbu:1:3) <> *zeros
144500130710      * se richiamato vado subito al subfile
144600130710     c                   eval      VCCfil = %dec(%subst(kpjbu:1:3):3:0)
144700130710     c                   eval      TipVid = '2'
144800130710     c                   eval      $carsfl = *on
144900130710     c                   ENDIF
145000020820      *
145100011109     C                   ENDSR
145200130703
145300130703      /free
145400130705       //--------------------------------------------------------------
145500130705       //?Copia da profilo STD devo richiedere filiale dell'utente.    ?
145600130705       //--------------------------------------------------------------
145700130705       BEGSR  GESW03;
145800130705
145900130705         clear W03fil;
146000130705         clear W03dfil;
146100130705         clear wFIL;
146200130705
146300130705         DOW  not *inkl and not *inkf;
146400130705           exfmt BS30W03;
146500130705           clear W03msg;
146600130705           *in28 = *off;
146700130705           SELECT;
146800130705         //?F12 = Ritorno;
146900130705             WHEN  *inkl;
147000130705         //?F06 = Conferma;
147100130705             WHEN  *inkf;
147200130705               exsr ctrFIL;
147300130705               IF  not *in28;
147400130709           //?se sto copiando uno STD NO COCOM passo l'utente così com'è
147500140710           //?stessa cosa per STD NO RDRED e REDREA
147600140710                 IF  HIDfun <> 'COCOM' and
147700140710                     HIDfun <> 'RED' and
147800140710                     HIDfun <> 'REA';
147900130709                   wUTE = VSCute;
148000130709                 ELSE;
148100130709           //?se sto copiando uno STD COCOM devo passare COM come utente
148200140710                   IF  HIDfun = 'COCOM';
148300130709                   wUTE = 'COM';
148400140710                   ENDIF;
148500140710             //?se sto copiando uno STD RDRED devo passare RED come utente
148600140710                   IF  HIDfun = 'RED';
148700140710                     wUTE = 'RED';
148800140710                   ENDIF;
148900140710             //?se sto copiando uno STD RAREA devo passare REA come utente
149000140710                   IF  HIDfun = 'REA';
149100140710                     wUTE = 'REA';
149200140710                   ENDIF;
149300130709                 ENDIF;
149400130705                 wFIL = W03fil;
149500130705                 exsr calltibs31;
149600130705               ENDIF;
149700130705             OTHER;
149800130705         //?ENTER
149900130705               exsr ctrFIL;
150000130705           ENDSL;
150100130705         ENDDO;
150200130705
150300130705       ENDSR;
150400160819
150500160819       //--------------------------------------------------------------
150600160822       //?Copia da profilo STD: devo richiedere filiale dell'utente    ?
150700160819       //?(e non solo...)                                              ?
150800160819       //--------------------------------------------------------------
150900160819       BEGSR  sr_GesWO3;
151000160822
151100160822         //*if  knmus <> 'EDPAC'  and
151200160822         //*    knmus <> 'EDPND'  and
151300160822         //*    knmus <> 'EDPGA'  and
151400160822         //*    knmus <> 'EDPMB'  and
151500160822         //*    knmus <> 'EDPLB';
151600160822         //*  *in28 = *on;
151700160822         //*  V1Msg = Msg(??);
151800160822         //*  leavesr;
151900160822         //*endif;
152000160822
152100160901         // -?"Costruzione" password?
152200160901         exsr  sr_NewPsw;
152300160901
152400160901         // -?Impostazione dati a video?
152500160819         clear  BS30WO3;
152600160901         W03utecpy = VSCute;
152700160819         W03sts = '*ENABLED ';
152800160819         W03scp = '*YES';
152900160901         W03psw = wPassword;
153000160819
153100160901         // -?Ciclo di emissione window WO3?
153200160822         SaveTipVid = TipVid;
153300160819         TipVid = '3';
153400160819
153500160819         DoW  TipVid = '3';
153600160819
153700160819           exfmt  BS30WO3;
153800160819
153900160819           clear  V1msg;
154000160819           *in28 = *off;
154100160819           *in45 = *off;
154200160819           *in46 = *off;
154300160819           *in47 = *off;
154400160819           *in48 = *off;
154500160819           *in49 = *off;
154600160822           *in50 = *off;
154700160819
154800160819           // -?F12 = Ritorno?
154900160819           if  *inKL;
155000160822             TipVid = SaveTipVid;
155100160819             leave;
155200160819           endif;
155300160819
155400160819           // -?Controllo Filiale per cui creare il profilo?
155500160819           exsr  CtrFIL;
155600160819           if  *in28;
155700160819             *in45 = *on;
155800160822             V1msg = W03msg;
155900160819             iter;
156000160819           endif;
156100160819           // -?Controllo Modulo Profilo?
156200160819           exsr  sr_CtrlModPrf;
156300160819           if  *in28  or  *in46;
156400160819             iter;
156500160819           endif;
156600160822           // -?Descrizione MB/AS?
156700160822           if  W03txt =  *blank;
156800160822             *in28 = *on;
156900160822             *in47 = *on;
157000160822             V1Msg = Msg(08);
157100160822             iter;
157200160822           endif;
157300160819           // -?Controllo Stato?
157400160822           if  W03sts <> '*ENABLED '  and
157500160822               W03sts <> '*DISABLED';
157600160819             *in28 = *on;
157700160822             *in48 = *on;
157800160822             V1Msg = Msg(09);
157900160822             iter;
158000160819           endif;
158100160819           // -?Controllo Scadenza Password?
158200160819           if  W03scp <> '*NO '  and
158300160822               W03scp <> '*YES';
158400160819             *in28 = *on;
158500160822             *in49 = *on;
158600160822             V1Msg = Msg(10);
158700160822             iter;
158800160819           endif;
158900160819           // -?Controllo Password?
159000160901           if  W03psw = *blank;
159100160819             *in28 = *on;
159200160822             *in50 = *on;
159300160822             V1Msg = Msg(11);
159400160822             iter;
159500160819           endif;
159600160819
159700160819           // -?F6 = Conferma?
159800160819           if  NOT *inKF;
159900160819             iter;
160000160819           endif;
160100160819
160200160819           Select;
160300160819             // -?SE sto copiando uno STD NO COCOM passo l'utente così com'è?
160400160819             //  ?stessa cosa per STD NO RDRED e REDREA?
160500160819             When  HIDfun <> 'COCOM'  and
160600160819                   HIDfun <> 'RED'    and
160700160819                   HIDfun <> 'REA';
160800160819               wUTE = VSCute;
160900160819             // -?SE sto copiando uno STD COCOM devo passare COM come utente?
161000160819             When  HIDfun = 'COCOM';
161100160819               wUTE = 'COM';
161200160819             // -?SE sto copiando uno STD RDRED devo passare RED come utente?
161300160819             When  HIDfun = 'RED';
161400160819               wUTE = 'RED';
161500160819             // -?SE sto copiando uno STD RAREA devo passare REA come utente?
161600160819             When  HIDfun = 'REA';
161700160819               wUTE = 'REA';
161800160819           EndSl;
161900160819
162000160819           // -?Richiamo *pgm di manutenzione?
162100160819           wFIL = W03fil;
162200160822           clear  TIBS31ds2;
162300160822           TIBS31ds2.I31moPr = W03moPr;
162400160822           TIBS31ds2.I31txt  = W03txt;
162500160822           TIBS31ds2.I31sts  = W03sts;
162600160822           TIBS31ds2.I31scp  = W03scp;
162700160822           TIBS31ds2.I31psw  = W03psw;
162800160819           exsr  CallTIBS31;
162900160822
163000160822           //*if  O31Err = *blank;
163100160822           //*  $CarSfl = '1';
163200160822           //*endif;
163300160822
163400160822           TipVid = SaveTipVid;
163500160822           leave;
163600160819
163700160819         EndDo;
163800160819
163900160819       ENDSR;
164000130705
164100130705       //--------------------------------------------------------------
164200130705       //?Controllo la filiale utente.
164300130705       //--------------------------------------------------------------
164400130705       BEGSR  CTRFIL;
164500130705
164600130705         clear W03dfil;
164700130705
164800130705         IF  W03fil = *zeros;
164900130705           W03msg = 'Immettere la filiale';
165000130705           *in28 = *on;
165100130705           leavesr;
165200130705         ENDIF;
165300130705
165400130705         chain W03fil AZORG01L;
165500130705         IF  not %found(AZORG01L) or ORGfva <> *blanks or
165600130705             (ORGfag <> 'F' and ORGfag <> 'A');
165700130705           W03msg = 'Filiale non valida';
165800130705           *in28 = *on;
165900130705           leavesr;
166000130705         ENDIF;
166100130705
166200130705         W03dfil = ORGdes;
166300130705
166400130705       ENDSR;
166500160819
166600160819       //--------------------------------------------------------------
166700160819       //?Controllo Modulo Profilo.                                    ?
166800160819       //--------------------------------------------------------------
166900160819       BEGSR  sr_CtrlModPrf;
167000160819
167100160819         Select;
167200160819
167300160823           // -?Non inserito nulla?
167400160823           When  W03moPr = *blank;
167500160823             //-*in28 = *on;
167600160823             //-*in46 = *on;
167700160823             //-V1Msg = Msg(06);
167800160823             //-leavesr;
167900160823             W03moPrD = *all'? ';
168000160819
168100160819           // -?Ricerca?
168200160822           When  %scan( '?' : W03moPr ) > *zero;
168300160822             clear  W03moPr;
168400160819             clear  TIBS02ds;
168500160819             T02mod = 'R';
168600160819             T02cod = 'MDP';
168700160819      /end-free
168800160819     C                   CALL      'TIBS02R'
168900160819     C                   PARM                    kpjba
169000160819     C                   PARM                    TIBS02ds
169100160819      /free
169200160819             if  T02err = *blank;
169300160822               W03moPr = T02ke1;
169400160819             endif;
169500160819             *in46 = *on;
169600160819             leavesr;
169700160819
169800160819           // -?Controllo?
169900160819           Other;
170000160819             clear  TIBS02ds;
170100160819             T02mod = 'C';
170200160819             T02cod = 'MDP';
170300160822             T02ke1 = W03moPr;
170400160819      /end-free
170500160819     C                   call      'TIBS02R'
170600160819     C                   parm                    KPJBA
170700160819     C                   parm                    TIBS02DS
170800160819      /free
170900160819             if  T02err <> *blank;
171000160819               *in28 = *on;
171100160819               *in46 = *on;
171200160819               V1Msg = Msg(06);
171300160819               leavesr;
171400160819             endif;
171500160822             W03moPrD = T02uni;
171600160822             if  W03moPr = 'S';
171700160822               *in28 = *on;
171800160822               *in46 = *on;
171900160822               V1Msg = %trimR( Msg(06) ) + ' - non inseribile';
172000160822               leavesr;
172100160822             endif;
172200160819
172300160819         EndSl;
172400160819
172500160819       ENDSR;
172600160822
172700160822       //--------------------------------------------------------------
172800160822       //?Creazione nuova password per l'utente in copia.
172900160822       //--------------------------------------------------------------
173000160822       BEGSR  sr_NewPsw;
173100160822
173200160822         clear  wPassword;
173300160822
173400160901         // ·?1° carattere = lettera reperita in base alla 1ª del?
173500160901         //  ?               nominativo?
173600160901         wP  = %scan( %subst( %xlate( c_Minusc : c_Maiusc : VSCtxt )
173700160901                              : 1 : 1 ) : c_Maiusc );
173800160822         wP += 8;
173900160822         if  wP > %elem( sk_alfab );
174000160822           wP = %rem( wP : %elem( sk_alfab ) );
174100160822         endif;
174200160822         wPassword = sk_alfab(wP);
174300160901
174400160901         // ·?2° carattere = lettera reperita in base alla 2ª del?
174500160901         //  ?               nominativo?
174600160901         wP  = %scan( %subst( %xlate( c_Minusc : c_Maiusc : VSCtxt )
174700160901                              : 2 : 1 ) : c_Maiusc );
174800160822         wP -= 8;
174900160822         select;
175000160822           when  wP < 1;
175100160822             wP += %elem( sk_alfab );
175200160822           when  wP > %elem( sk_alfab );
175300160901             wP  = %rem( wP : %elem( sk_alfab ) );
175400160822         endsl;
175500160901         wPassword = %trimr( wPassword ) + sk_alfab(wP);
175600160901
175700160822         // ·?3°-5° carattere = millisecondi attuali?
175800160822         //        ?(eventuale sostituzione della prima cifra SE *zero)?
175900160822         // ·?6°-9° carattere = secondi attuali elevati al quadrato?
176000160901         wP  = ( %subdt( %timestamp() : *ms ) * 10 ) +
176100160901               ( %subdt( %timestamp() : *s ) *
176200160901                 %subdt( %timestamp() : *s ) );
176300160822         if  wP  < 1000000;
176400160822           wP += 9000000;
176500160822         endif;
176600160901         wPassword = %trimr( wPassword ) + %editc( wP : 'X' );
176700160822
176800160822       ENDSR;
176900130705
177000130703       //--------------------------------------------------------------
177100130703       //?Carico abilitazioni per profili standard.                    ?
177200130703       //--------------------------------------------------------------
177300130703       BEGSR  ABISTD;
177400130703
177500130704         wEnd = *off;
177600130703         clear xx;
177700130703         clear skUTE;
177800130703         clear skABI;
177900130703         clear skAUT;
178000130703         clear skFUN;
178100130703
178200130703       //?Carico gli utenti standard in SK + abilitazioni
178300130703         exec sql
178400130703         DECLARE AZUTE cursor for
178500130703         SELECT * from AZUTE00F
178600130703         ORDER BY UTEute;
178700130703
178800130703       //?Apertura del cursore
178900130703         exec sql
179000130703         OPEN AZUTE;
179100130703
179200130703         IF  sqlcode < 0;
179300130703           wEnd = *on;
179400130703         ENDIF;
179500130703
179600130703         DOW  not wEnd;
179700130703           exec sql
179800130703           FETCH next from AZUTE into :azuteds;
179900130703           IF  sqlcod = 100 or sqlcod < 0;
180000130703             wEnd = *on;
180100130703             leave;
180200130703           ENDIF;
180300130703
180400130703       //?solo utenti standard
180500130703           dUte02 = UTEfa1;
180600130930           dUte01 = UTEfaf;
180700130703           IF  §UT2mopr = 'S';
180800130703             xx += 1;
180900130703             skUTE(xx) = UTEute;
181000130703             skABI(xx) = UTEfaf;
181100130703             skAUT(xx) = UTEaut;
181200130703             skFUN(xx) = UTEfun;
181300130703           ENDIF;
181400130703
181500130703         ENDDO;
181600130703
181700130703       //?Chiusura del cursore
181800130703         exec sql
181900130703         CLOSE AZUTE;
182000130703
182100130703       ENDSR;
182200130703
182300130703      /end-free
182400130703
182500020820      *----------------------------------------------------------------
182600020820      *  Reperisco Dati del job (Utente/Operativi)
182700020820      *----------------------------------------------------------------
182800020820     C     DatiJob       BegSr
182900020820      *
183000020820     C     *dtaara       define    §azute        azuteds
183100020820     C     *dtaara       define    §datiute      ddatiute
183200020820      *
183300020820     C                   in(E)     *dtaara
183400020820     C                   IF        %Error or RSUT = *blanks
183500020820     C                   call      'TIBS34R'
183600020820     C                   parm                    Tibs34Ds
183700020820     C                   in        *dtaara
183800020820     C                   ENDIF
183900020820      *
184000020820     C                   endsr
184100011109      *--------------------------------------------------------------
184200120213
184300120213     P*--------------------------------------------------
184400120213     P* Procedure name: GetProfiloUtenteProj
184500120213     P* Purpose:        Restituisce l'esistenza del profilo utente in Proj.
184600120213     P* Returns:        Profilo utente in Proj.
184700120213     P* Parameter:      priProfiloUtente => Profilo Utente
184800120213     P*--------------------------------------------------
184900120213     P GetProfiloUtenteProj...
185000120213     P                 B
185100120213     D GetProfiloUtenteProj...
185200120213     D                 PI             1A
185300120213     D  priProfiloUtente...
185400120213     D                               10A
185500120213
185600120213     D retField        S              1A   STATIC
185700120213
185800120213      /FREE
185900120213
186000120213       CLEAR retField;
186100120213
186200120213       EXEC SQL
186300120217         SELECT CASE
186400120217                WHEN utnAnn = '1' THEN 'A'
186500120217                WHEN utnGrp = 999 THEN 'D'
186600120217                ELSE 'S'
186700120217                END
186800120213           INTO :retField
186900120213           FROM anutn00f
187000120213           WHERE utnPrf = :priProfiloUtente
187100120213       ;
187200120213
187300120213       SELECT;
187400120213         WHEN sqlCode < *ZERO;
187500120213           DUMP(A);
187600120213           retField = '?';
187700120213       ENDSL;
187800120213
187900120213       RETURN retField;
188000120213
188100120213      /END-FREE
188200120213     P GetProfiloUtenteProj...
188300120213     P                 E
188400140522
188500140522      *----------------------------------------------------
188600140523      * Procedure name: IsFilMess
188700140523      * Purpose:        Filiale utente Messaggeria
188800140522      * Returns:        ON - OFF
188900140522      *----------------------------------------------------
189000140523     p IsFilMess       B
189100140523     d IsFilMess       PI              n
189200140522
189300140522     d isErrore        s               n   inz
189400140522
189500140522      /free
189600140522
189700140522       clear OG140;
189800140522       clear OG143;
189900140522       chain (UTEfil) AZORG01L;
190000140522       IF  %found(AZORG01L);
190100140522         OG140 = ORGde0;
190200140522         OG143 = ORGde3;
190300140522       ENDIF;
190400140522
190500140523       IF og143.§OGntw = 'MES' and og140.§OGapo <> %editc(ORGfil:'X');
190600140522         return *on;
190700140522       ELSE;
190800140522         return *off;
190900140522       ENDIF;
191000140522
191100140522      /end-free
191200140523     p IsFilMess       E
191300140522
191400160822** - sk_alfab +....2....+.
191500160822ABCDEFGHIJKLMNOPQRSTUVWXYZ
191600160822** -- MSG - Lungh. 78 -------------------------------------------------------*
191700130709TIBS30R- Filiale inesistente
191800011109TIBS30R- Funzione non corretta
191900011109TIBS30R- Autorità non corretta
192000011109TIBS30R- Opzione non corretta
192100011130Scorrimento indietro o in avanti oltre il primo o l'ultimo record del file.
192200130704TIBS30R- Tipo Modulo non corretto
192300130704TIBS30R- Immettere la filiale se richiesto Ordinamento per Cod.Utente
192400160822TIBS30R- Descrizione obbligatoria                                              8
192500160822TIBS30R- Stato del profilo non corretto (*ENABLED/*DISABLED)                   9
192600160822TIBS30R- Scadenza Password non corretta (*NO/*YES)                            10
192700160822TIBS30R- Password del profilo obbligatoria                                    11
