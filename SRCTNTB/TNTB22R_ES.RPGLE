000100120207     /*PRM  dbgview(*source)
000200120207     /*END
000300120207     h debug decedit('0,') datedit(*dmy/) option(*nodebugio)
000400120207     h dftactgrp(*no)
000500120207     h bnddir('TRUL')
000600020207      *--------------------------------------------------------------------------------------------*
000700020207      *        GESTIONE TABELLA 3NN PER AUTORIZZAZIONI VCONFERMA BOLLE                             *
000800020207      *--------------------------------------------------------------------------------------------*
000900110805      *?  ATTENZIONE!!  ?
001000110805      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001100110805      *?    'Interrogazione abilitazioni clienti'              ?
001200110805      *?    In caso di aggiunta/modifica campi alla tabella    ?
001300110805      *?    verificare se sono validi per la visualizzazione   ?
001400110805      *?    da TA61                                            ?
001500020207      *--------------------------------------------------------------------------------------------*
001600020207      * Data base                                                                                  *
001700020207      *--------------------------------------------------------------------------------------------*
001800020207     FTnTb22d   CF   E             WORKSTN
001900020207     F                                     SFILE(tb22s1:nrr)
002000020207     Ftntbe01l  IF   E           K DISK
002100020207     Ftntbe02l  IF   E           K DISK    rename(tntbe000:tntbe002)
002200020207     FAzOrg01l  IF   E           K DISK
002300160317     Ftabel00f  IF   E           K DISK
002400120207
002500120207       //--------------------------------------------------------------
002600120207       //?Definizione costanti.                                        ?
002700120207       //--------------------------------------------------------------
002800120207
002900120207       // -?Supporto Cliente a Bartolini "EasySpedWEB"?
003000120207     d c_Supp_ESWEB    c                   const('ESWEB')
003100120207
003200020207      *--------------------------------------------------------------------------------------------*
003300020207      * Data structure                                                                             *
003400020207      *--------------------------------------------------------------------------------------------*
003500020207      *---
003600020207      * Schiere a tempo di compilazione
003700020207      *---
003800160317     D msg             S             78    DIM(5) CTDATA PERRCD(1)              *Messaggi video
003900020207      *---
004000020207      * Schiere memorizzazione dati correnti
004100020207      *---
004200020207      * Variabili
004300020207      *---
004400020207     D Nrr             S              4  0                                      *n° rel sf
004500120207     D $Fine           S               n   INZ                                  *fine programma
004600020207     D TipVid          S              1    INZ('1')                             *video in gestione
004700020207     D $CarSfl         S              1    INZ('0')                             *video in gestione
004800020207     D $CarVd1         S              1    INZ('1')                             *video in gestione
004900020207      *---
005000020207      * Indici
005100020207      *---
005200020207     D i               S              5  0 INZ                                  *indice
005300020207     D j               S              5  0 INZ                                  *indice
005400020207     D k               S              5  0 INZ                                  *indice
005500020207     D g               S              5  0 INZ                                  *indice
005600020207     D v               S              5  0 INZ                                  *indice
005700020207     D m               S              5  0 INZ                                  *indice
005800160317     D codut           S              1  0 INZ(1)
005900160317     D wforza          S              1    INZ
006000160317     D kcod            S                   like(tblcod)
006100160317     D kkey            S                   like(tblkey)
006200160317     D W1Cod           S                   like(C1COld)
006300020207     D WMax            S                   like($nrr)
006400011010     D Win             S             99A
006500020207      *---
006600020207      * Ds
006700020207      *---
006800020207     D Tntb22Ds      E DS                                                       *ds di procedura
006900020207     D                                     INZ                                  *ds di procedura
007000020207     D Tibs02Ds      E DS                                                       *ds di procedura
007100020207     D Tibs69Ds      E DS                                                       *ds di procedura
007200020207     D Og143         E DS                                                       *ds di procedura
007300020207     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
007400020207     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
007500020207     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
007600020207     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
007700020207      *
007800020207      *---
007900020207      * architettura
008000020207      *---
008100020207     D kpjba         E DS                                                       *architettura
008200110805     d Tnta61ds      e ds
008300120207
008400120207       // -?Tabelle usate:?
008500120207       // ·?"3C" = Clienti che inviano bolle partenza?
008600120207     d ds3C          e ds                  inz  qualified
008700120207       // ·?"3EW" = Dati assegnati alla postazione EasyWeb?
008800120207     d d3EW          e ds                  inz  qualified
008900120207
009000120207       // -?Status ds?
009100120207     d Status         sds
009200120207     d  SDSpgm           *proc
009300120207
009400120207       //--------------------------------------------------------------
009500120207       //?Definizione aree dati.                                       ?
009600120207       //--------------------------------------------------------------
009700120207
009800120207       // -?Dati utente?
009900120207     d §AzUte        e ds                  extname(AZUTE00F)
010000120207     d                                     dtaara
010100120207     d §DatiUte      e ds                  extname(dDatiUte)
010200120207     d                                     dtaara
010300120207
010400120207       //--------------------------------------------------------------
010500120207       //?Definizione prototipi procedure.                             ?
010600120207       //--------------------------------------------------------------
010700120207
010800120207       // -?Reperimento dati utente?
010900120207     d TIBS34ds      e ds                  inz
011000120207      /copy gaitrasrc/srcProtoPR,TIBS34R
011100120207
011200120207       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
011300120207     d c_TABEL         c                   const('1')
011400120207       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
011500120207     d c_TNTBE         c                   const('2')
011600120207       // -?Reperimento dati tabelle?
011700120207      /copy gaitrasrc/srcProtoPR,TRULTAB
011800120207
011900120207       // -?Verifica abilitazione STRATEGI?
012000120207      /copy gaitrasrc/srcProtoPI,TIS7701R
012100120207     d TIVSSds       e ds                  extname(TIVSS00F) qualified
012200120207      /copy gaitrasrc/srcProtoPR,TIS7701R
012300120207
012400020207      *--------------------------------------------------------------------------------------------*
012500020207      * Main lines
012600020207      *--------------------------------------------------------------------------------------------*
012700020207      *
012800020207      * Operazioni iniziali
012900020207      *
013000020207     C                   EXSR      RutInz
013100110805
013200110805     c                   eval      *in10 = *on
013300110805     c                   Select
013400110805     c                   When      %parms() > 1  and  TA61ksc <= *zero
013500110805     c                   eval      TA61err = *on
013600110805     c                   eval      TA61msg = 'Richiesto il codice cliente'
013700120207     c                   eval      $Fine = *on
013800110805     c                   When      %parms() > 1
013900110805     c                   eval      V1Ccod  = %editc(TA61ksc:'X')
014000110805     c                   eval      *in10 = *off
014100110805     c                   eval      TipVid = '2'
014200110805     c                   eval      $CarSfl = *on
014300110805     c                   eval      $CarVd1 = *off
014400110805     C                   move      v1ccod        $Cod
014500110805     C                   EXSR      DeCod
014600110805     c                   EndSl
014700020207      *
014800020207      * Gestione video
014900020207      *
015000120207     C     $Fine         DOUEQ     *on
015100020207     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
015200020207     C     TipVid        CASEQ     '2'           GesSfl                          *Sfile
015300010921     C                   ENDCS
015400010921     C                   ENDDO
015500020207      *
015600010921     C                   SETON                                        LR
015700020207      *--------------------------------------------------------------------------------------------*
015800020207      * GesVd1 - Gestione videata selezione codice tabella -
015900020207      *--------------------------------------------------------------------------------------------*
016000020207     C     GesVd1        BEGSR
016100020207      *
016200020207      * Inizializzazione videata
016300020207     C     $CarVd1       ifeq      *on
016400020207     C                   EXSR      CarVd1
016500020207     C                   move      *off          $CarVd1
016600020207     C                   endif
016700020207      *
016800120208     c                   if        Not *in28
016900120207     C                   write     TB22T1
017000120208     c                   endif
017100020208     C                   EXFMT     Tb22v1
017200020208      *
017300020208     C                   EXSR      CtrSearch
017400020207      *
017500020207     C                   select
017600020207      * F3=Fine
017700020207     C     *inKc         wheneq    *on
017800020207     C                   EXSR      F03Ges
017900020207      *
018000020207     C     *in55         wheneq    *on
018100020207     C                   EXSR      Search
018200020207     C                   other
018300020207      *
018400020207      * Controlli e decodifiche
018500020208     C                   EXSR      CtrV1
018600020207      *
018700020207     C     *IN28         ifeq      *off
018800020211     C     Ric_Est       andeq     *off
018900020207      *
019000020207      * Gestisco il video di dettaglio
019100020207     C                   move      *on           $CarSfl
019200020207     C                   move      '2'           TipVid
019300160317     c                   clear                   wforza
019400020207     C                   endif
019500020207      *
019600020207     C                   endsl
019700020207      *
019800020207      *
019900020207     C                   endsr
020000020207      *--------------------------------------------------------------------------------------------*
020100020207      * CarVd1 - Carica primo video
020200020207      *--------------------------------------------------------------------------------------------*
020300020207     C     CarVd1        BEGSR
020400020207      *
020500020207     C                   setoff                                       284041
020600020211      *
020700020207     C                   clear                   Tb22v1
020800020207     C                   movel     '?      '     V1CCod
020900020207      *
021000020207     C                   ENDSR
021100020207      *--------------------------------------------------------------------------------------------*
021200020207      * Search - Ricerca su campi video
021300020207      *--------------------------------------------------------------------------------------------*
021400020207     C     Search        BEGSR
021500020208     C                   setoff                                       284041
021600020207      *
021700020208      * Il P.O deve essere controllato
021800020208     C     v1cpou        chain     Azorg01l                           21
021900020208     C     v1cpou        ifeq      *zeros
022000020208     C     *in21         oreq      *off
022100020207      *
022200020207     C                   move      *blanks       V1DCod
022300020207     C                   move      *blanks       Tibs02Ds
022400020207     C                   move      '3NN'         T02Cod
022500020211      *
022600020211      * Gli come chiave parziale per le ricerche il P.O.
022700020211     C                   move      *blanks       t02ke1
022800020211     C                   movel     v1cpou        t02ke1
022900020207     C                   move      'R'           T02Mod
023000020207     C                   movel     Knsif         T02Sif
023100020207     C                   CALL      'TIBS02R'
023200020207     C                   PARM                    kpjba
023300020207     C                   PARM                    Tibs02Ds
023400020207      *
023500020207      * per valorizzare a video devo aver selezionato un codice
023600020207     C                   if        T02Err = *blanks and T02ke1<>*zeros
023700020208     C                   movel     T02ke1        v1ccod
023800020208      *
023900020208      * decodifico
024000020208     C                   move      v1ccod        $Cod              7 0
024100020208     C                   EXSR      DeCod
024200020208      *
024300020208     C                   if        o69err = *blanks and acoflg =' '
024400020208     C                   movel     acorag        v1dcod
024500020208     C                   endif
024600020207     C                   endif
024700020208     C                   endif
024800020207      *
024900020207     C                   ENDSR
025000020207      *--------------------------------------------------------------------------------------------*
025100020207      * Controllo e decodifica prima videata
025200020207      *--------------------------------------------------------------------------------------------*
025300020208     C     CtrV1         BEGSR
025400020207      *
025500020208     C                   setoff                                       284041
025600020211     C                   move      *off          Ric_Est           1
025700020207     C                   clear                   V1CMsg
025800020207      *
025900020208      * se avevo richiesto la ricerca effettuo subito i controlli su P.O.
026000020208 1a  C     *in56         ifeq      *on
026100020208 2a  C     v1cpou        ifne      *zeros
026200020208     C     v1cpou        chain     Azorg01l                           21
026300020208     C   21              seton                                        2841
026400020208     C   21              goto      FineSr
026500020208 2-  C                   endif
026600020208 1-  C                   endif
026700020208      *
026800020208 1a  C     v1ccod        ifeq      *blanks
026900020207      *
027000020207      * Ricerca estesa
027100020207     C
027200120207     C                   movel     rsut          paxdut           30
027300020207     C                   movel(P)  v1dcod        paxdmt           48
027400020207      *
027500020207     C                   z-add     0             paxsta
027600120207     C                   z-add     DUTkci        paxccc            4 0
027700020207     C                   clear                   paxdit
027800020207     C                   z-add     1             paxnum
027900020207      *
028000020207     C                   CALL      'XALFA3BR'
028100020207     C                   PARM                    PAXDUT
028200020207     C                   PARM                    CODUT
028300020207     C                   PARM                    PAXDMT
028400020207     C                   PARM                    PAXCCC
028500020207     C                   PARM                    PAXSTA            1 0
028600020207     C                   PARM                    PAXFLR           90
028700020207     C                   PARM                    PAXDIT            3
028800020207     C                   PARM                    PAXNUM            2 0
028900020207     C                   PARM                    PAXKCM           80
029000020207     C                   PARM                    PAXKSM          140
029100020207     C                   PARM                    PAXKDM           60
029200020207      *
0293000202082a   C     PaxSta        ifgt      -1
029400020207     C                   movel     paxksm        v1ccod
029500020207     C                   movel     paxdmt        v1dcod
0296000202082-   C                   endif
029700020211      * sehgnalo che provengo da ricerca estesa per
029800020211      * riemettere video.
029900020211     C                   move      *on           Ric_Est           1
030000020208     C                   goto      Finesr
0301000202081-   C                   endif
030200020207      *
030300020207      * Codice cliente obbligatorio
030400020208     C                   movel     *blanks       v1dcod
0305000202081a   C     v1ccod        ifeq      *blanks
030600020208     C                   seton                                        2840
030700020207      *
030800020207      * Se esiste controllo e decodifico
0309000202081e   C                   else
031000020208      * Controllo se il codice è numerico
031100020208     C                   testn                   v1ccod               31
031200020208     C                   move      v1ccod        w001a             1
0313000202082a   C     *in31         ifeq      *on
031400020208     C     w001a         andge     '0'
031500020208     C                   move      v1ccod        $Cod              7 0
031600020208     C                   EXSR      DeCod
031700020208      *
0318000202083a   C                   if        o69err = *blanks and acoflg =' '
031900020207     C                   movel     acorag        v1dcod
0320000202083e   C                   else
032100020208     C                   seton                                        2840
0322000202083-   C                   endif
032300020208      *
0324000202082e   C                   else
032500020208     C                   seton                                        2840
0326000202082-   C                   endif
0327000202081-   C                   endif
032800020207      *
032900020208     C     FineSr        ENDSR
033000020207      *--------------------------------------------------------------------------------------------*
033100020207      * Gessfl - gestione video subfile
033200020207      *--------------------------------------------------------------------------------------------*
033300020207     C     GesSfl        BEGSR
033400010924      *
033500010924      * azzero indicatori
033600020207     C     $CarSfl       ifeq      *on
033700020207     C                   exsr      CarSfl
033800010921     C                   endif
033900010927      *
034000120207      * scrivo la testata
034100120208     c                   if        Not *in28
034200120207     c                   write     TB22T1
034300120208     c                   endif
034400010927      * scrivo il piede
034500020207     C                   write     tb22z1
034600011017      * se non ci sono dati da caricare e non ho richesto il posizionamento
034700010927     C     wmax          ifeq      0
034800020207     C                   seton                                        71
034900020207     C                   setoff                                       70
035000020207     C                   write     tb22v2                                       NO RCD
035100011017     C                   endif
035200010927      * emetto il control
035300020207     C                   exfmt     tb22c1
035400160317     c                   setoff                                       2890
035500020207      *
035600020207     C                   select
035700020207      *
035800020207      * F03=Fine
035900020207     C     *inkc         wheneq    *on
036000160317     c                   exsr      ctr5F
036100160317     C  n28              EXSR      F03Ges
036200020207      *
036300020207      * F10=Immissione
036400020207     C     *inkj         wheneq    *on
036500020211     C                   EXSR      F10Ges
036600160317      * F07=Gestione tabella 5F
036700160317     C     *inkg         wheneq    *on
036800160317     c                   clear                   kpjbu
036900160317     c                   call      'TRTB35R'
037000160317     c                   parm                    kpjba
037100160317     c                   eval      wforza='1'
037200020207      *
037300020207      * F12=Ritorno
037400020207     C     *inkl         wheneq    *on
037500160317     c                   exsr      ctr5F
037600160317     c
037700160317     C  n28              EXSR      F12Ges
037800020207     C                   EndSl
037900010928      *
038000020207      * Ho qualcosa di caricato da controllare
038100160317     C  n28wmax          ifgt      *zeros
038200020207     C                   EXSR      CtrS1
038300010928     C                   endif
038400020207      *
038500010921     C                   ENDSR
038600020207      *--------------------------------------------------------------------------------------------*
038700020207      * CarSfl - caricamento sfile
038800020207      *--------------------------------------------------------------------------------------------*
038900020207     C     CarSfl        BEGSR
039000020207      *
039100010928     C                   setoff                                           90
039200020207     C                   z-add     *zeros        Nrr                            *n° record sfile
039300020207     C                   z-add     *zeros        c1cold                         *n° record sfile
039400020208     C                   move      *blanks       c1dold                         *n° record sfile
039500020207      * pulizia sfile
039600020207     C                   seton                                        7071      *non visualizza sfl
039700020207     C                   write     tb22c1
039800020207     C                   setoff                                       7071
039900020207      *
040000020207      * caricamento sfile -tutto-
040100020207     C                   EXSR      ScrSfl                                       *scrive sfile
040200020207      *
040300010921     C                   ENDSR
040400020207      *--------------------------------------------------------------------------------------------*
040500020207      * ScrSfl - scrive il sfile -tutto-
040600020207      *--------------------------------------------------------------------------------------------*
040700020207     C     ScrSfl        BEGSR
040800120207      *
040900120207      /free
041000120207
041100120207         // -?Reperimento tab. "3C"?
041200120207         clear  ds3C;
041300120207         clear  d3EW;
041400120207         ds_TNTBE.TBEke1 = V1Ccod;
041500120207         IF  getTabella ( c_Tabel : '3C'  : ds_TNTBE.TBEke1 :
041600120207                          *omit : *omit : *omit :
041700120207                          *omit : *omit :
041800120207                          *omit : *omit : *omit : *omit :
041900120207                          *omit : *omit : *omit : *omit :
042000120207                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
042100120207                        = *zero      AND
042200120207             ds_TNTBE.TBEatb = *blank;
042300120207           ds3C   = ds_TNTBE.TBEuni;
042400120207           If  ds3C.§3Ccba = c_Supp_ESWEB;
042500120207             // -?Reperimento abilitazione a Strategi (SUN)?
042600120418             //rqsKSU = '0' + V1Ccod;
042700120418             rqsKSU = '0' + %editc( ds3C.§3Ccks : 'X' );
042800120207             clear  rqsSUN;
042900120207             clear  rpyEsito;
043000120207             Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
043100120207                                      rqsSUN : *zero : TIVSSds :
043200120207                                      rpyEsito );
043300120207             // -?Reperimento tab. "3EW"?
043400120418             //ds_TNTBE.TBEke1 = '0' + V1Ccod;
043500120418             ds_TNTBE.TBEke1 = '0' + %editc( ds3C.§3Ccks : 'X' );
043600120207             clear  ds_TNTBE.TBEke2;
043700120207             if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
043800120207               ds_TNTBE.TBEke2 = TIVSSds.VSSsun;
043900120207             endif;
044000120207             if  getTabella ( c_Tntbe : '3EW' : ds_TNTBE.TBEke1 :
044100120207                              ds_TNTBE.TBEke2 : *blank : *blank :
044200120207                              *omit : *omit :
044300120207                              *omit : *omit : *omit : *omit :
044400120207                              *omit : *omit : *omit : *omit :
044500120207                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
044600120207                            = *zero      AND
044700120207                 ds_TNTBE.TBEatb = *blank;
044800120207               d3EW = ds_TNTBE.TBEuni;
044900120207             endif;
045000120207           EndIf;
045100120207         ENDIF;
045200120207
045300120207      /end-free
045400020207      *
045500020207      * imposta dati
045600020207     C                   move      v1ccod        c1cold
045700020208     C                   movel     v1dcod        c1dold
045800120816     c                   if        ds3C.§3Ccba  = c_Supp_ESWEB
045900120816     c                   if        d3EW.§3EWfaa = *blank
046000120207     c                   eval      C1Cfls = d3EW.§3EWfls
046100120816     c*//                eval      C1Cnrs = d3EW.§3EWnrs
046200120816     c                   endif
046300120207     c                   else
046400120207     c                   eval      C1Cfls = ds3C.§3Cfls
046500120816     c*//                eval      C1Cnrs = ds3C.§3Cnrs
046600120816     c                   endif
046700120816     c                   eval      C1Cnrs = ds3C.§3Cnrs
046800010927     C                   z-add     0             wmax
046900020207      *
047000020207     C                   move      '3NN'         tbecod
047100020208     C                   move      *blanks       tbelin
047200020207     C                   movel     $sif          tbesif
047300020207     C                   move      *blanks       tbeke1
047400020207     C                   movel     v1ccod        tbeke1
047500010927      *
047600010927      * mi posiziono anche per codice
047700020207     C     k02tbe4       setll     tntbe02l
047800020207     C     k02tbe4       reade     tntbe02l                               97
047900010924      *
048000010924      * Ciclo di lettura sulla tabella da caricare
048100020207     C                   dow       not *in97
048200010924      *
048300010924      * riempimento campi subfile
048400020207     C                   exsr      RieS1
048500011017      * imposto attributi su sfl
048600011017     C                   Exsr      AtrS1
048700010924      * scrive sfile
048800010924     C                   add       1             nrr                            *incrementa n°rec
048900010927      *
049000010927      * mi posiziono sul primo record caricato
049100010927     C                   z-add     1             $nrr                           *posiziona in fondo
049200020207     C                   write     tb22s1                                       *scrive sfile
049300020207     C                   z-add     nrr           wmax
049400010921      *
049500020207     C     k02tbe4       reade     tntbe02l                               97
049600020207     C                   ENDDO
049700020207     C                   move      '0'           $CarSfl
049800020207      *
049900010921     C                   ENDSR
050000020207      *--------------------------------------------------------------------------------------------*
050100020207      * F03ges - Tasto funzionale f03 -> Fine programma
050200020207      *--------------------------------------------------------------------------------------------*
050300020207     C     F03ges        BEGSR
050400020207      *
050500120207     C                   movel     *on           $Fine                          *fine pgm
050600020207      *
050700010921     C                   ENDSR
050800020211      *--------------------------------------------------------------------------------------------*
050900020211      * F10ges - Tasto funzionale f10 -> Immissione
051000020211      *--------------------------------------------------------------------------------------------*
051100020211     C     F10ges        BEGSR
051200020211      *
051300020211     C                   reset                   Tntb22ds
051400020214     C                   move      *blanks       b22ke1
051500020214     C                   movel     c1cold        b22ke1
051600020211     C                   eval      B22Fun='F10'
051700020211     C                   EXSR      callb23r
051800020211      *
051900020211     C                   move      *on           $CarSfl
052000020211     C                   z-add     1             $nrr                           *posiziona in fondo
052100020211      *
052200020211     C                   ENDSR
052300020207      *--------------------------------------------------------------------------------------------*
052400020207      * F12ges - Tasto funzionale f12 -> Ritorno
052500020207      *--------------------------------------------------------------------------------------------*
052600020207     C     F12ges        BEGSR
052700020207      *
052800110805     c                   IF        *in10
052900020207     C                   move      '1'           TipVid
053000020212     C                   move      *on           $CarVd1
053100110805     c                   else
053200120207     C                   movel     *on           $Fine                          *fine pgm
053300110805     c                   endif
053400020207      *
053500020207     C                   ENDSR
053600160317      *--------------------------------------------------------------------------------------------*
053700160317     c     Ctr5f         BEGSR
053800160317     c
053900160317     c                   if        wforza=' ' and wmax>0
054000160317     c* verifico se non è presenete la 5F --> errore forzabile
054100160317     c                   eval      kcod='5F'
054200160317     c                   eval      kkey='   ' +%editc(c1cfls:'X')+
054300160317     c                                        %editc(c1cnrs:'X')
054400160317     c     ktab          chain     tabel00f
054500160317     c                   if        not %found(tabel00f)
054600160317     c                   eval      c1cmsg=msg(5)
054700160317     c                   else
054800160317     c                   eval      c1cmsg=msg(4)
054900160317     c                   endif
055000160317
055100160317     c                   seton                                        2890
055200160317     c                   z-add     1             nrr
055300160317     c                   eval      wforza='1'
055400160317     C     nrr           chain     tb22s1                             97        *per sposizionamento
055500160317     c                   exsr      aggsfl
055600160317
055700160317     c                   endif
055800160317
055900160317     C                   ENDSR
056000020207      *--------------------------------------------------------------------------------------------*
056100160317      * callb23r
056200020207      *--------------------------------------------------------------------------------------------*
056300020207     C     callb23r      BEGSR
056400010924      *
056500020207      * valorizzo Ds di richiamo
056600010924     C
056700020207     C                   eval      b22lin=' '
056800020207     C                   eval      b22sif=tbesif
056900010924      *
057000010924      * Richiamo pgm di gestione
057100020207     C                   call      'TNTB23R'
057200010925     C                   PARM                    kpjba
057300020207     C                   PARM                    tntb22ds
057400010924      *
057500120207     C     b22fun        ifeq      'F03'
057600020207     C     b22err        oreq      *on
057700120207     C                   eval      $Fine = *on
057800120207     c                   else
057900120207     c                   if        C1Cnrs > *zero
058000120207     c                   clear                   kpjbu
058100120207     c                   call      'TRTB35R'
058200120207     c                   parm                    kpjba
058300120207     c                   endif
058400120207     C                   endif
058500160317
058600160317     c                   movel     '1'           wforza
058700020207      *
058800010924     C                   ENDSR
058900020207      *--------------------------------------------------------------------------------------------*
059000020207      * CtrS1  - Controlla campi sfile
059100020207      *--------------------------------------------------------------------------------------------*
059200020207     C     CtrS1         BEGSR
059300020207      *
059400011008     C                   setoff                                         9082
059500020208     C                   setoff                                         2842
059600010924      *
059700010924      * lettura dei record "toccati" del sfile
059800010924     C                   z-add     1             nrr
059900020207     C                   readc     tb22s1                                 82
060000010927     C                   dow       not *in82
060100010924      *
060200010924      * gestione opzioni
060300010924      *
060400010924      * Opzioni
060500020207     C                   if        s1copz <> ' '
060600010924      *
060700020207     C                   if        s1copz = '2'  and s1cAnn <>'A'or
060800020207     C                             s1copz = '4'  and s1cAnn <>'A'or
060900020207     C                             s1copz = '5'  or
061000020207     C                             s1copz = '7'  and s1cAnn ='A'
061100010926      *
061200020207     C                   reset                   tntb22ds
061300020207     C                   eval      b22opz=s1copz
061400020207      * campi chiave
061500020207     C                   move      *blanks       b22ke1
061600020207     C                   movel     c1cold        b22ke1
061700020207     C                   move      *blanks       b22ke2
061800020207     C                   movel     s1cpou        b22ke2
061900010924      *
062000020211      * Gestione richiamo tntb23r
062100020207     C                   EXSR      callb23r
062200020211      * ricarico il subfile
062300020211     C                   eval      $carsfl='1'
062400020211      * mi posiziono sul record da cui ho selezionato l'opzione
062500020211     C                   z-add     nrr           $nrr                           *posiziona in fondo
062600010924      *
062700020207     C                   else
062800010927      * c'e un errore sulle opzioni
062900020207     C                   seton                                        9082
063000020208     C                   seton                                        2842
063100020207     C                   exsr      ErrOpz
063200020207     C                   endif
063300020208     C                   endif
063400020207      *
063500020207      * rilegge sfile per sposizionamento
063600020207     C     nrr           chain     tb22s1                             97        *per sposizionamento
063700020207      *
063800020207      * aggiorna sfile
063900020207     C                   EXSR      aggsfl                                       *aggiorna sfile
064000020207      *
064100020207      * lettura successiva record
064200020207     C                   readc     tb22s1                                 82
064300010927     C                   enddo                                                  *fine sfile
064400020207      *
064500000000     C                   ENDSR
064600020207      *--------------------------------------------------------------------------------------------*
064700010927      * Errori su scelta opzione
064800020207      *--------------------------------------------------------------------------------------------*
064900010927     C     Erropz        BEGSR
065000020207      *
065100020207     C                   clear                   c1cmsg
065200020208      *
065300020207     C                   if        s1copz <>'2'  and s1copz <>'4'and
065400020207     C                             s1copz <>'5'  and s1copz <>'7'
065500020208     C                   eval      c1cmsg  = msg(1)
065600020207     C                   endif
065700010928      *
065800020207     C                   if        s1copz = '7'
065900020208     C                   eval      c1cmsg  = msg(2)
066000020207     C                   endif
066100010928      *
066200020207     C                   if        s1copz = '2'  or  s1copz = '4'
066300020208     C                   eval      c1cmsg  = msg(3)
066400020207     C                   endif
066500020207      *
066600020207     C                   ENDSR
066700020207      *--------------------------------------------------------------------------------------------*
066800020207      * aggsfl - aggiorna riga di sfile
066900020207      *--------------------------------------------------------------------------------------------*
067000000000     C     aggsfl        BEGSR
067100010924      * aggiorna sfile
067200020207     C                   Z-ADD     nrr           $nrr                           *posizionamento
067300020208     C                   seton                                        70
067400020208     C  n28              clear                   s1copz
067500011017      *
067600020215     C                   move      s1hflg        *in43
067700020207     C                   update    tb22s1
067800020207     C                   setoff                                       70
067900020207      *
068000000000     C                   ENDSR
068100020207      *--------------------------------------------------------------------------------------------*
068200020207      * RieS1  - vlorizzo i campi del subfile
068300020207      *--------------------------------------------------------------------------------------------*
068400020207     C     Ries1         BEGSR
068500020207      *
068600020208      * Nuovo codice cliente e decodifica
068700020207     C                   setoff                                       4041
068800020211     C                   move      ' '           s1copz
068900030704     C*    TbeUni        ifne      *blanks
069000030704     c                   IF        %SUBST(tbeuni:1:7) <> *blanks  and
069100030704     c                             %SUBST(tbeuni:1:7) <> *zeros
069200020208     C                   movel     tbeuni        s1cnew
069300020214     C                   else
069400020214     C                   move      *blanks       s1cnew
069500020214     C                   endif
069600020208      *
069700020208      * Se esiste controllo e decodifico
069800020214     C                   move      *blanks       s1dnew
069900020215     C     s1cnew        ifne      *blanks
070000020208     C                   move      s1cnew        $Cod              7 0
070100020208     C                   EXSR      DeCod
070200020208      *
070300020208     C                   if        o69err = *blanks and acoflg =' '
070400020208     C                   movel     acorag        s1dnew
070500020208     C                   endif
070600020215     C                   endif
070700020208      *
070800020208      * Punto operativo e decodifica
070900020207     C                   movel     tbeke2        s1cpou
071000020214     C                   movel     *blanks       s1dpou
071100020214     C     s1cpou        ifne      '999'
071200020207     C                   move      s1cpou        $Fil              3 0
071300020207     C     $Fil          chain     Azorg01l                           21
071400020207     C                   movel     orgdes        s1dpou
071500020214     C                   else
071600020214     C                   movel     'Tutti'       s1dpou
071700020214     C                   endif
071800010926      *
071900010921     C                   ENDSR
072000020207      *--------------------------------------------------------------------------------------------*
072100020207      * AtrS1  - Ripristino giusti attributi su sfl
072200020207      *--------------------------------------------------------------------------------------------*
072300011017     C     AtrS1         BEGSR
072400011017      *
072500020207      * record annullato
072600020207     C     tbeatb        ifeq      'A'
072700020207     C                   movel     'A'           S1CAnn
072800020208     C                   seton                                        43
072900020215     C                   move      *on           s1hflg
073000020207     C                   else
073100020207     C                   movel     ' '           S1CAnn
073200020208     C                   setoff                                       43
073300020215     C                   move      *off          s1hflg
073400020207     C                   endif
073500011017      *
073600020207     C                   ENDSR
073700020208      *--------------------------------------------------------------------------------------------*
073800020208      * DeCod  - Decodifica codice cliente
073900020208      *--------------------------------------------------------------------------------------------*
074000020208     C     DeCod         BEGSR
074100020208      *
074200020208     C                   clear                   tibs69ds
074300120207     C                   z-add     DUTkci        I69Kcc
074400020208     C                   movel     $Cod          I69Kac
074500020208     C                   movel     KnSif         I69Sif
074600020208     C                   call      'TIBS69R'
074700020208     C                   parm                    Tibs69Ds
074800020208     C                   parm                    DsAco
074900020208     C                   parm                    DsInd
075000020208     C                   parm                    DsClp
075100020208     C                   parm                    DsCls
075200020208      *
075300020208     C                   ENDSR
075400020208      *--------------------------------------------------------------------------------------------*
075500020208      * CtrSearch - Controlli prima delle ricerche
075600020208      *--------------------------------------------------------------------------------------------*
075700020208     C     Ctrsearch     BEGSR
075800020208     C                   setoff                                       5556
075900020208      *
076000020208      * Ricerca solo se P.O a zero o codice valido
076100020208     C     '?'           scan      V1CCod                                 55
076200020208     C     *in55         ifeq      *on
076300020208     C     v1cpou        andne     *zeros
076400020208      *
076500020208      * segnalo che ho richiesto la ricerca
076600020208     C                   seton                                        56
076700020208     C     v1cpou        chain     Azorg01l                           21
076800020208     C   21              setoff                                       55
076900020208     C                   endif
077000020208      *
077100020208     C                   ENDSR
077200020207      *--------------------------------------------------------------------------------------------*
077300020207      * RutInz - operazioni iniziali
077400020207      *--------------------------------------------------------------------------------------------*
077500000210     C     rutinz        BEGSR
077600020207      *
077700020207      * Ricevimento parametri
077800000000     C     *ENTRY        PLIST
077900000000     C                   PARM                    kpjba
078000110805     c                   parm                    TNTA61ds
078100020207      *
078200120207     C                   z-add     1             CodUt             1 0
078300120207      /free
078400120207
078500120207         // -?Impostazione nome programma a video?
078600120207         V1Tpgm = SDSpgm;
078700120207
078800120207         // -?Reperimento Dati del job (Utente/Operativi).?
078900120207         in(E) §AzUte;
079000120207         if NOT %error;
079100120207           in(E) §DatiUte;
079200120207         endif;
079300120207         if %error or RSut = *blanks;
079400120207           clear TIBS34ds;
079500120207           tibs34r ( tibs34ds );
079600120207           in §AzUte;
079700120207           in §DatiUte;
079800120207         endif;
079900120207
080000120207      /end-free
080100020207      *
080200020207      * Recupero S.I.
080300020207      *
080400020207     C                   MOVEL     *blanks       tbecod                         *tabella
080500020207     C                   MOVEL     *all'0'       tbeke1                         *chiave uno
080600020207     C                   MOVE      '3NN'         tbeke1
080700020207     C     k01tbe2       chain     tntbe01l                           97
080800020207     C  N97              movel     tbesif        $sif             10
080900120207     C   97              move      *on           $Fine
081000020207      *
081100020207      *
081200020207      * chiavi di lettura
081300020207     C     K01tbe2       KLIST                                                  *tntbe01l
081400020207     C                   KFLD                    tbecod                          -tabella
081500020207     C                   KFLD                    tbeke1                          -chiave uno
081600020207      *
081700020207     C     K01tbe5       KLIST                                                  *tntbe01l
081800020207     C                   KFLD                    tbecod                          -tabella
081900020207     C                   KFLD                    tbeke1                          -chiave uno
082000020207     C                   KFLD                    tbeke2                          -chiave due
082100020207     C                   KFLD                    tbelin                          -lingua
082200020207     C                   KFLD                    tbesif                          -s.informativo
082300010921      *
082400020207     C     k02tbe4       KLIST                                                  *tntbe01l
082500020207     C                   KFLD                    tbecod                          -tabella
082600020207     C                   KFLD                    tbelin                          -chiave uno
082700020207     C                   KFLD                    tbesif                          -chiave due
082800020207     C                   KFLD                    tbeke1                          -lingua
082900010924      *
083000020207     C     k02tbe3       KLIST                                                  *tntbe01l
083100020207     C                   KFLD                    tbecod                          -tabella
083200020207     C                   KFLD                    tbelin                          -chiave uno
083300020207     C                   KFLD                    tbesif                          -chiave due
083400160317     C     Ktab          KLIST                                                  *tntbe01l
083500160317     C                   KFLD                    codut                           -tabella
083600160317     C                   KFLD                    kcod
083700160317     C                   KFLD                    kkey
083800010924      *
083900020207     C                   ENDSR
084000020207      *--------------------------------------------------------------
084100010927** MSG  Lungh. 78                                                            *
084200020212Opzione non valida
084300020212Impossibile ripristinare un record già valido
084400020212Record annullato
084500160317Verificare le filiali della 5F con il tasto funzionale F7. Enter per forzare
084600160317Non presente tabella 5F: INSERIRLA tramite il tasto funzionale F7
