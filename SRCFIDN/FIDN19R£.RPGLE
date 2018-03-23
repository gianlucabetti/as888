000100980506      *------------------------------------------------------------------------*
000200981001      * Stampa denuncia eventi
000300980506      *------------------------------------------------------------------------*
000400980922      *   USO BOLLO stampare dalla riga 11 alla 59 e dalla colonna 16 alla 93
000500980922      *------------------------------------------------------------------------*
000600981001      *  A T T E N Z I O N E  ! ! !
000700981001      *    se viene modificato il numero degli elementi della schiera del testo da
000800981001      *    stampare AGGIORNARE la schiera COSC con il nuovo numero finale di riga.
000900981001      *    Attenzione anche alle sostituzioni condizionate dall'ora denuncia.
001000981001      *------------------------------------------------------------------------*
001100980729
001200980729      ****************************************************************
001300980729      *  RIEPILOGO INDICATORI
001400980729      ***************************************************************
001500980915      * 01   - SALTO PAGINA
001600980911      * 30   - Generico
001700980915      * 31   - Generico
001800980915      * 32   - Generico
001900980729      ****************************************************************
002000980729
002100980730     H DECEDIT('0,') DATEDIT(*DMY/)
002200980729
002300981001     fFIDN18D   CF   E             WORKSTN  sfile(FI18S02:nrr1)
002400980923     fFNDCT01L  UF   E           K DISK
002500981001     fTNTMD01L  IF   E           K DISK
002600990806     fTITAS30C  IF   E           K DISK
002700980917     fFNDCS01L  IF   E           K DISK
002800980923     fFNDCF01L  UF A E           K DISK
002900981001     fFNDET01L  UF   E           K DISK
003000980922     fPRTF198   O    F  198        PRINTER  oflind(*INOF)  infds(PRTFBK)  usropn
003100980729
003200980914      *----------------------------------
003300980915     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG VIDEO
003400981001     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
003500981001     D COTE            S              1    DIM(2) CTDATA PERRCD(1)              codice testo
003600981001     D COSC            s              3S 0 DIM(2) ALT(cote)                     coordinate schiera
003700981001     D LET             S            100    DIM(4) CTDATA PERRCD(1)              testo lettera
003800990223     D KLE             S            100    DIM(7) CTDATA PERRCD(1)              testata sped.lettera
003900981001     D BOL             S             79    DIM(5) CTDATA PERRCD(1)              testo uso bollo
004000990223     D KBO             S             79    DIM(7) CTDATA PERRCD(1)              testata sped. bollo
004100980915      *----------------------------------
004200980730     D KBAR            C                   CONST('BARTOLINI  S.p.a.')
004300980730     D KSDI            C                   CONST('S.D.I.  S.p.a.')
004400980914     D MAI             C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004500980914     D MIN             C                   CONST('abcdefghijklmnopqrstuvwxyz')
004600980922     D KMANP           C                   CONST('MANCANZA PARZIALE')
004700980922     D KMANT           C                   CONST('MANCANZA TOTALE')
004800980922     D KAMMA           C                   CONST('AMMANCO')
004900980729      *----------------------------------
005000980616     D                SDS
005100980616     D  VTCPGM                 1     10
005200980730      *----------------------------------
005300980730     D WLBDAT          DS                  INZ
005400980730     D  g02dat                 1      8  0
005500980730     D  g02inv                 9     16  0
005600980730     D  g02err                17     17
005700980730     D  g02tgi                18     22  0
005800980914      *
005900980914     D WDATAalfa       DS                  INZ
006000980914     D  WAAalfa                1      4
006100980914     D  WMMalfa                5      6
006200980914     D  WGGalfa                7      8
006300980914      *
006400980914     D WORAalfa        DS                  INZ
006500980914     D  WoraHH                 1      2
006600980914     D  WoraMM                 3      4
006700980915      *
006800980915     D PARAM4          DS
006900980915     D  PA4CNU                 1      3  0
007000980917      *
007100980917     D WNUMCA          DS
007200980917     D  V2HAAC
007300980917     D  V2CFIL
007400980917     D  V2CNCA
007500980922      *
007600980922     D PRTFBK          DS
007700980922     D  cur_line             367    368I 0
007800980915      *----------------------------------
007900980921     D FIDN05DS      E DS
008000990205     D FIDN18DS      E DS
008100990205     D FIDN20DS      E DS
008200980507     D KPJBA         E DS
008300010905     D*** DSTD          E DS
008400981229     D DDCT01        E DS
008500980915     D UT§DSE0F      E DS
008600980915     D TIBS02DS      E DS
008700981001     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
008800981001     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
008900990806     D DS_TITAS      E DS                  EXTNAME(TITAS00F)
009000980729      *----------------------------------
009100980922     D delett          S            100
009200980922     D deboll          S             79
009300980922     D trat100         S            100    INZ(*all'-')
009400980922     D trat79          S             79    INZ(*all'-')
009500980921     D command         S             80
009600980917     D w005a           S              5
009700980917     D w007a           S              7
009800980917     D w007b           S              7
009900990223     D wpzman          S             11
010000990223     D wcoman          S             11
010100990223     D wkgman          S             11
010200980915     D wsocie          S             20
010300990212     D wprimaca        S              1
010400980923     D wmaxcapag       s              2  0
010500980923     D wcaprt          s              2  0
010600980917     D XX              s              3  0
010700981001     D YY              s              3  0
010800980730     D dateu           s              8  0
010900980730     D wdtgio          S              8  0
011000980921     D w0080           S              8  0
011100980921     D wora            S              4  0
011200980730     D w0140           S             14  0
011300980915     D nrr1            S              4  0
011400980915     D nrr2            S              4  0
011500990119     D lung            S             15  5 inz(80)
011600980916     D DATA_oggi       S               D   DATFMT(*eur)
011700980917     D DATA_eur        S               D   DATFMT(*eur)
011800980730      *
011900980917     D kaas            S                   LIKE(DCTaas)
012000980917     D kfil            S                   LIKE(DCTfil)
012100980917     D klnp            S                   LIKE(DCTlnp)
012200981001     D kaac            S                   LIKE(DCTaac)
012300980917     D knca            S                   LIKE(DCTnca)
012400980917     D knrs            S                   LIKE(DCTnrs)
012500980917     D knsp            S                   LIKE(DCTnsp)
012600980917     D ktpd            S                   LIKE(DCStpd)
012700980917     D knkt            S                   LIKE(DCSnkt)
012800980917     D kstd            S                   LIKE(DCSstd)
012900980921      *
013000981117     D Paad            S                   LIKE(DCTaad)
013100980921     D Ppod            S                   LIKE(DCTpod)
013200980921     D Pnde            S                   LIKE(DCTnde)
013300981007     D Paac2           S              2S 0
013400981007     D Pfil            S                   LIKE(DCTfil)
013500981007     D Pnca            S                   LIKE(DCTnca)
013600990223     D Prag            S                   LIKE(V1aurs)
013700990223     D Pind            S                   LIKE(V1auin)
013800990223     D Pcap            S                   LIKE(V1auca)
013900990223     D Ploc            S                   LIKE(V1aulo)
014000990223     D Ppro            S                   LIKE(V1aupr)
014100980921     D Plnp            S                   LIKE(DCTlnp)
014200980921     D Pnrs            S                   LIKE(DCTnrs)
014300980921     D Pnsp            S                   LIKE(DCTnsp)
014400980921     D Pdts            S              6  0
014500981001     D Pncl            S                   LIKE(TASncl)
014600990806     D Ppkf            S                   LIKE(TASpkF)
014700981001     D Pprd            S                   LIKE(TMDprd)
014800981001     D Pnas            S                   LIKE(DCTnas)
014900981007     D PLErsm          S             30
015000981007     D PLErsd          S             30
015100981007     D PLElod          S             30
015200981007     D PBOrsm          S             24
015300981007     D PBOrsd          S             24
015400981007     D PBOlod          S             23
015500990223     D Pdes            S             35
015600990223     D PLEdespz2       S             99
015700990223     D PBOdespz2       S             78
015800990212
015900990223     d DS_DESPZ        DS
016000040330     ***D W2300A                      2300
016100040330     ***D DPM                           35    DIM(60)  overlay(W2300A:1)        Descr. Pezzi Mancan.
016200040330     ***d PLEdespz1                     62             overlay(W2300A:1)
016300040330     ***d LEdepz                        99    dim(21)  overlay(W2300A:63)
016400040330     ***d PBOdespz1                     41             overlay(W2300A:1)
016500040330     ***d BOdepz                        78    dim(27)  overlay(W2300A:42)
016600040330     d W2300A                     32600
016700040330     d  DPM                          35    DIM(930) overlay(W2300A:1)           Descr. Pezzi Mancan.
016800040330     d  PLEdespz1                    62             overlay(W2300A:1)
016900040330     d  LEdepz                       99    dim(328) overlay(W2300A:63)
017000040330     d  PBOdespz1                    41             overlay(W2300A:1)
017100040330     d  BOdepz                       78    dim(417) overlay(W2300A:42)
017200980921
017300980731      *---------------------------------------------------------------------
017400980910
017500990223      * dati autorità
017600980911     c                   EXSR      RIEdatiau
017700990127      *
017800990127      * Aggancio FNDET e imposto N. denuncia
017900990127     c     KDET          CHAIN     FNDET000                           30
018000990127     C                   z-add     DETaad        Paad
018100990127     C                   z-add     DETpod        Ppod
018200990127     C                   z-add     DETnde        Pnde
018300980914
018400980923      * Imposto tipo modulo e massimo numero c.a. per pagina per dettaglio denuncia
018500990223     c                   if        V1mode = 'B'
018600980914     c                   movea     CMD(1)        COMMAND
018700980923     c                   z-add     7             wmaxcapag
018800980914     c                   else
018900980917     c                   movea     CMD(2)        COMMAND
019000980923     c                   z-add     11            wmaxcapag
019100980914     c                   endif
019200980923      *
019300980914     c                   call      'QCMDEXC'
019400980914     c                   parm                    COMMAND
019500980914     c                   parm                    LUNG
019600980914
019700980914      * Apro file stampa testo denuncia
019800980914     c                   OPEN      PRTF198
019900980914
020000981001     c                   z-add     1             YY
020100990223     c     V1mode        lookup    COTE(YY)                               30
020200981001
020300990223     c                   if        V1mode = 'L'
020400980911      * Stampo testo denuncia tipo lettera
020500980911     c                   EXSR      PRTdelet
020600980911     c                   else
020700980911      * Stampo testo denuncia tipo uso bollo
020800980911     c                   EXSR      PRTdebol
020900980911     c                   endif
021000980914
021100980910      * Stampo allegato
021200980915     c                   EXSR      PRTalleg
021300980922
021400980922      * Chiudo file stampa testo denuncia
021500980922     c                   CLOSE     PRTF198
021600980910
021700981029     c                   clear                   tibs02ds
021800981029     c                   movel     'C'           t02tla
021900981029     c                   call      'TIBS02R'
022000981029     c                   parm                    kpjba
022100981029     c                   parm                    tibs02ds
022200980506     C                   EVAL      *INLR = *ON
022300980911      *****************************************************************
022400980911      *   IMPOSTO DATI AUTORITA'
022500980911      *****************************************************************
022600980911     C     RIEdatiau     BEGSR
022700990223      *
022800990223     c                   z-add     1             nrr2
022900990223     c     NRR2          CHAIN     FI18S02                            32
023000990223      *
023100990223     c                   movel     V1aurs        Prag
023200990223     c                   movel     V1auin        Pind
023300990223     c                   movel     V1auca        Pcap
023400990223     c                   movel     V1aulo        Ploc
023500990223     c                   movel     V1aupr        Ppro
023600980911      *
023700980911     c                   ENDSR
023800980729      *****************************************************************
023900980911      *   STAMPO TESTO DENUNCIA MODULO LETTERA
024000980729      *****************************************************************
024100980911     C     PRTdelet      BEGSR
024200980730
024300980914      *  Stampo autorità
024400980914     C                   EXCEPT    EXLEautor
024500980914
024600980914      *  Spazio dall'autorità al testo
024700980914     c                   DO        4             XX
024800980914     C                   EXCEPT    EXLEspazio
024900980914     c                   ENDDO
025000980731
025100980914      *  Stampo testo
025200981001     c                   DO        COSC(YY)      XX
025300980921     c                   MOVEL(P)  LET(XX)       delett
025400980914     c                   EXSR      impvarlet
025500980914     C                   EXCEPT    EXLEdetlet
025600980914     c                   ENDDO
025700980914
025800980914      *  Stampa firma
025900980914     C                   EXCEPT    EXLEfirma
026000980914
026100980730     C                   ENDSR
026200980804      *****************************************************************
026300980914      *   IMPOSTO VARIABILI TESTO DENUNCIA  ** LETTERA **
026400980804      *****************************************************************
026500980914     C     IMPVARLET     BEGSR
026600980804      *
026700981001     c                   IF        XX = 1
026800981001     c                   eval      %subst(delett:40:35) = I18prd
026900981001     c                   movel     I18dpr        Wdataalfa
027000981001     c                   eval      %subst(delett:84:10)  =
027100980914     c                                 (WGGalfa + '/' + WMMalfa + '/' + WAAalfa)
027200981001     c                   ENDIF
027300980914      *
027400980914     C                   ENDSR
027500980914      *****************************************************************
027600980914      *   STAMPO TESTO DENUNCIA MODULO USO BOLLO
027700980914      *****************************************************************
027800980914     C     PRTdebol      BEGSR
027900980914
028000980914      *  Stampo autorità
028100980914     C                   EXCEPT    EXBOautor
028200980914
028300980914      *  Stampo testo
028400981001     c                   DO        COSC(YY)      XX
028500980921     c                   MOVEL(P)  BOL(XX)       deboll
028600980914     c                   EXSR      impvarbol
028700980914     C                   EXCEPT    EXBOdetbol
028800980914     c                   ENDDO
028900980914
029000980914      *  Stampa firma
029100980915     C                   EXCEPT    EXBOfirma
029200980914
029300980914     C                   ENDSR
029400980914      *****************************************************************
029500980914      *   IMPOSTO VARIABILI TESTO DENUNCIA  ** BOLLO **
029600980914      *****************************************************************
029700980914     C     IMPVARBOL     BEGSR
029800980914      *
029900981117     c                   IF        XX = 1
030000981001     c                   eval      %subst(deboll:40:35) = I18prd
030100981117     c                   ENDIF
030200980914      *
030300981117     c                   IF        XX = 2
030400981001     c                   movel     I18dpr        Wdataalfa
030500981001     c                   eval      %subst(deboll:9:10)  =
030600981001     c                                 (WGGalfa + '/' + WMMalfa + '/' + WAAalfa)
030700981117     c                   ENDIF
030800980914      *
030900980914     C                   ENDSR
031000980915      *****************************************************************
031100980915      *   STAMPO ALLEGATO
031200980915      *****************************************************************
031300980915     C     PRTALLEG      BEGSR
031400980915      *
031500980915     c                   z-add     1             nrr2
031600980915      *
031700981001     c     NRR2          CHAIN     FI18S02                            32
031800980915      *
031900980915     c     *IN32         doweq     *OFF
032000980915      *
032100981001      * Elaboro se in fase corretta e scelta non bloccata
032200990223     c                   IF        V2HI59 <> '1'  and  V2HI58 <> '1'
032300980915      *
032400990127      * Se non ancora impostato il n° denuncia lo ricavo e aggiorno FNDET
032500990127     c                   if        Pnde = *zeros
032600980915     c                   EXSR      NUMDEN
032700980923     c                   EXSR      AGGFNDET
032800980915     c                   endif
032900980921      *
033000980922      * Imposto dati righe dettaglio
033100980917     c                   EXSR      RIEDET
033200990212      *
033300990212      * Per la prima CA salto a pagina nuova
033400990212     c                   IF           Wprimaca = *blanks
033500990212     c                   eval      Wprimaca = 'S'
033600990223     c                   If        V1mode = 'L'
033700990212     c                   except    EXLEtall
033800990212     c                   Else
033900990212     c                   except    EXBOtall
034000990212     c                   Endif
034100990212     c                   ENDIF
034200990212      *
034300980917      * Stampo
034400990212     c                   exsr      PRTca
034500980921      *
034600980921      * Aggiorno files
034700040330     c                   if        KNMUS <> *all'1'
034800980923     c                   EXSR      AGGFNDCT
034900980923     c                   EXSR      AGGFNDCF
035000040330     c                   endif
035100980917      *
035200980915     c                   ENDIF
035300980915      *
035400980915     c                   add       1             nrr2
035500981001     c     NRR2          CHAIN     FI18S02                            32
035600980915     c                   enddo
035700980915      *
035800980922      * Stampo tratteggio di chiusura
035900990223     c                   If        V1mode = 'L'
036000980922     c                   EXCEPT    EXLEfine
036100980922     c                   else
036200980922     C  N01              eval      *in01 = (CUR_line = 59)
036300980922     c                   EXCEPT    EXBOfine
036400980922     c                   endif
036500980922      *
036600980915     C                   ENDSR
036700980915      *****************************************************************
036800980915      *   STACCO NUMERO DENUNCIA
036900980915      *****************************************************************
037000980915     C     NUMDEN        BEGSR
037100990205
037200990205      * Il controllo dell'esistenza del numeratore è effettuato dal PGM di lancio
037300990205
037400990205     C                   clear                   FIDN20DS
037500990205     C                   movel     'N'           I20mod
037600990205     C                   z-add     DETaae        I20pra
037700990205     C                   movel     'DEN'         I20dit
037800990205      *
037900990205     C                   movel(P)  FIDN20DS      KPJBU
038000990205      *
038100990205     c                   CALL      'FIDN20R'
038200990205     c                   PARM                    KPJBA
038300990205      *
038400990205     C                   movel     KPJBU         FIDN20DS
038500990205      *
038600990205     c                   IF          O20err <> 'E'
038700990205     C                   z-add     DETaae        PAAD
038800990205     C                   Z-ADD     I18FGS        Ppod
038900990205     C                   Z-ADD     O20prn        Pnde
039000990205     C                   ENDIF
039100990205
039200980915     C                   ENDSR
039300980915      *****************************************************************
039400980916      *   RIEMPIO I CAMPI DEL DETTAGLIO
039500980915      *****************************************************************
039600980917     C     RIEDET        BEGSR
039700980917      *
039800980915      * Dati spedizione
039900980915     c                   z-add     V2Haas        Kaas
040000980915     c                   z-add     V2Clnp        Klnp
040100980915     c                   z-add     V2Cnrs        Knrs
040200980915     c                   z-add     V2Cnsp        Knsp
040300980915      *
040400990806     c                   clear                   DS_TITAS
040500990806     c     KSPE          chain     TITAS30C                           30
040600981001     c                   clear                   DS_tntmd
040700981001     c     KSPE          chain     TNTMD000                           30
040800981001      *
040900981001     c                   Z-ADD     V2Clnp        Plnp
041000981001     c                   Z-ADD     V2Cnrs        Pnrs
041100981001     c                   Z-ADD     V2Cnsp        Pnsp
041200981001     c                   z-add     V2Haas        w0080
041300981001     c                   movel     TASmgs        w0080
041400981001     c                   if        TASmgs > *zeros
041500981001     c     *USA          move      w0080         DATA_eur
041600981001     c     *dmy          MOVE      DATA_eur      Pdts
041700981001     c                   endif
041800981001     c                   Z-ADD     TASncl        Pncl
041900990806     c                   Z-ADD     TASpkF        Ppkf
042000981001     c                   MOVEL     TMDprd        Pprd
042100980921      *
042200990223     c                   If        V1mode = 'L'
042300981001     c                   MOVEL     TMDrsm        PLErsm
042400981001     c                   MOVEL     TMDrsd        PLErsd
042500981001     c                   MOVEL     TMDlod        PLElod
042600980921     c                   else
042700981001     c                   MOVEL     TMDrsm        PBOrsm
042800981001     c                   MOVEL     TMDrsd        PBOrsd
042900981001     c                   MOVEL     TMDlod        PBOlod
043000980921     c                   endif
043100980917      *
043200980915      * Dati C.A.
043300980917     c                   z-add     V2Haac        Kaac
043400980917     c                   z-add     V2Cfil        Kfil
043500980917     c                   z-add     V2Cnca        Knca
043600980915     C     kdct          CHAIN     fndct000                           31
043700981229     c                   movel     DCTflo        DDCT01
043800981229     c                   movel     DCTnas        Pnas
043900981007     c                   z-add     V2Haac        Paac2
044000981007     c                   z-add     V2Cfil        Pfil
044100981007     c                   z-add     V2Cnca        Pnca
044200980917      *
044300981001      * Reperisco dati per descrizione danno
044400990223     c                   movel     *blanks       wpzman
044500981001     c                   movel     *blanks       wcoman
044600981001     c                   movel     *blanks       wkgman
044700990223     C                   clear                   Pdes
044800980917     c                   clear                   XX
044900980917      *
045000981001      * Kg danneggiati
045100981117     c                   IF        DCTpkd > *zeros
045200980917     c                   move(P)   DCTpkd        w007a
045300980917     c     '0'           check     w007a         xx
045400980917     c                   eval      W007b = %subst(w007a:xx)
045500980922     c     ' '           checkr    w007b         xx
045600980917     c                   if        xx = 1
045700981104     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
045800981104     c                   else
045900981104     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(xx-1))
046000981104     c                                      +  ','  +  %subst(w007b:XX:1)
046100980917     c                   endif
046200980917     c                   ENDIF
046300980917      *
046400981001      * Colli danneggiati
046500990223     c                   IF        DCTncn > *zeros
046600980917     c                   move      DCTncn        w005a
046700980917     c     '0'           check     w005a         xx
046800990223     c                   eval      Wcoman = 'Colli '  +  %subst(W005a:XX)
046900990223     c                   ENDIF
047000980917      *
047100981001      * Pezzi danneggiati
047200990223     c                   IF        DCTnpz > *zeros
047300980917     c                   move      DCTnpz        w005a
047400980917     c     '0'           check     w005a         xx
047500990223     c                   eval      Wpzman = 'Pezzi '  +  %subst(W005a:XX)
047600990223     c                   ENDIF
047700980917      *
047800990223     c                   eval      Pdes =     %trim(WKGman)  +  '  '
047900990223     c                                     +  %trim(WCOman)  +  '  '
048000990223     c                                     +  %trim(WPZman)
048100990212      *
048200990223      * Descrizione pezzi mancanti
048300990309     c                   clear                   W2300A
048400990223     c                   IF          DCTnpz > *zeros
048500990212     C                   movel(P)  'C'           Ktpd
048600990212     C                   movel(P)  Wnumca        Knkt
048700990223     C                   movel(P)  'P'           Kstd
048800990212     c                   clear                   XX
048900990212      *
049000990212     c     kdcs          chain     fndcs000                           30
049100990212      *
049200040330     ***c     *in30         DOWeq     *off
049300040330     ***c     xx            ANDlt     60
049400040330     c                   DOW           *in30 = *off
049500040330     c                             and xx    < %elem(DPM)
049600990212     c                   add       1             XX
049700990223     c                   movel     dcsnot        DPM(XX)
049800990212     c     kdcs          reade     fndcs000                               30
049900990212     c                   ENDDO
050000990223      *
050100990223     c                   ENDIF
050200980917      *
050300980915     C                   ENDSR
050400990212      *****************************************************************
050500990212      *   STAMPA CA IN DENUNCIA
050600990212      *****************************************************************
050700990212     C     PRTca         BEGSR
050800990212      *
050900990212     c                   exsr      PRTsalto
051000990223     c                   If        V1mode = 'L'
051100990212     c                   except    EXLEdall01
051200990212     c                   Else
051300990212     c                   except    EXBOdall01
051400990212     c                   Endif
051500990212      *
051600990212     c                   exsr      PRTsalto
051700990223     c                   If        V1mode = 'L'
051800990212     c                   except    EXLEdall02
051900990212     c                   Else
052000990212     c                   except    EXBOdall02
052100990212     c                   Endif
052200990212      *
052300990212     c                   exsr      PRTsalto
052400990223     c                   If        V1mode = 'L'
052500990212     c                   except    EXLEdall03
052600990212     c                   Else
052700990212     c                   except    EXBOdall03
052800990212     c                   Endif
052900990212      *
053000990223     c                   If        V1mode = 'L'
053100990223     c                   clear                   PLEdespz2
053200040330     ***c                   DO        21            XX
053300040330     c                   clear                   xx
053400040330     c                   DOw       XX < %elem(LEdepz)
053500040330     c                   add       1             xx
053600990223     c                   If        LEdepz(XX) <> *blanks
053700990223     c                   movel     LEdepz(XX)    PLEdespz2
053800990212     c                   exsr      PRTsalto
053900990212     c                   except    EXLEdall04
054000990212     c                   Endif
054100990212     c                   ENDDO
054200990212      *
054300990212     c                   ELSE
054400990223     c                   clear                   PBOdespz2
054500040330     ***c                   DO        27            XX
054600040330     c                   clear                   xx
054700040330     c                   DOw       XX < %elem(BOdepz)
054800040330     c                   add       1             xx
054900990223     c                   If        BOdepz(XX) <> *blanks
055000990223     c                   movel     BOdepz(XX)    PBOdespz2
055100990212     c                   exsr      PRTsalto
055200990212     c                   except    EXBOdall04
055300990212     c                   Endif
055400990212     c                   ENDDO
055500990212     c                   ENDIF
055600990212      *
055700990212     C                   ENDSR
055800990212      *****************************************************************
055900990212      *   SALTO PAGINA
056000990212      *****************************************************************
056100990212     C     PRTSALTO      BEGSR
056200990212      *
056300990212     c                   SELECT
056400990212     c                   WHEN      CUR_line <  59
056500990223     c                   WHEN      V1mode = 'L'
056600990212     c                   except    EXLEtall
056700990223     c                   WHEN      V1mode = 'B'
056800990212     c                   except    EXBOtall
056900990212     c                   ENDSL
057000990212      *
057100990212     C                   ENDSR
057200980922      *****************************************************************
057300980922      *   SCRIVO FNDCF FASI C.A.
057400980922      *****************************************************************
057500980923     C     AGGFNDCF      BEGSR
057600980923      *
057700980923     C                   z-add     DCTaaC        DCFaac
057800980923     C                   z-add     DCTfil        DCFfil
057900980923     C                   z-add     DCTnca        DCFnca
058000981001     c                   z-add     I18fca        DCFfca
058100980923     c                   z-add     dateu         DCFdfc
058200980923     c                   z-add     wora          DCFhfc
058300981001     c                   z-add     I18fgs        DCFfev
058400980923     c                   movel     knmus         DCFpru
058500980923      *
058600980923     c                   WRITE     FNDCF000
058700980923      *
058800980923     C                   ENDSR
058900980921      *****************************************************************
059000980921      *   AGGIORNO FNDCT TESTATA C.A.
059100980921      *****************************************************************
059200980923     C     AGGFNDCT      BEGSR
059300980921      *
059400980921      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
059500980923     c                   CLEAR                   FIDN05DS
059600980923     c                   movel     'F'           I05MOD
059700981001     c                   z-add     I18fca        I05fca
059800980923     c                   movel     'P'           I05fpr
059900980923     c                   movel     DCTptr        I05tpc
060000980923     c                   movel     'E'           I05fpe
060100981030     c                   movel     'D'           I05fde
060200980923     c                   movel     'O'           I05ffs
060300980923     c                   z-add     dateu         i05dta
060400980923     c                   move      DCTtad        i05tad
060500981125      * Valorizzo numero CA
060600981125     c                   z-add     DCTaac        i05aac
060700981125     c                   z-add     DCTfil        i05fil
060800981125     c                   z-add     DCTnca        i05nca
060900980921
061000980923     C                   CALL      'FIDN05R'
061100980923     c                   PARM                    KPJBA
061200980923     c                   PARM                    FIDN05DS
061300980921
061400980923     c                   IF        O05err = *blanks
061500980923     c                   movel     O05REC        DS_FNDFA
061600980923     c                   SELECT
061700980923     c                   WHEN      DFAgfs = 'P'
061800981229     c                   movel     §DCTlnpc      dctgfc
061900980923     c                   WHEN      DFAgfs = 'S'
062000980923     c                   z-add     46            dctgfc
062100981023     c                   WHEN      DFAgfs = 'A'
062200981023     c                   z-add     DCTlna        dctgfc
062300980923     c                   OTHER
062400981023     c                   z-add     DCTfil        dctgfc
062500980923     c                   ENDSL
062600980923     c                   ENDIF
062700980921      *
062800980921      * Imposto campi per trasmissione dati
062900981001     c                   z-add     I18fca        DCTfca
063000981117     C                   z-add     Paad          DCTaad
063100980923     C                   z-add     Ppod          DCTpod
063200980923     C                   z-add     Pnde          DCTnde
063300981222     c                   clear                   DCTft1
063400981222     c                   clear                   DCTft2
063500980921      *
063600980923     c                   EXCEPT    UPDdct
063700980921      *
063800980923     C                   ENDSR
063900980921      *****************************************************************
064000981001      *   AGGIORNO FNDET TESTATA DENUNCIA
064100980921      *****************************************************************
064200980923     C     AGGFNDET      BEGSR
064300981001      *
064400981117     C                   z-add     Paad          DETaad
064500980923     C                   z-add     Ppod          DETpod
064600981117     C                   z-add     Pnde          DETnde
064700980923     c                   z-add     dateu         DETdsb
064800980921      *
064900981001     c                   UPDATE    FNDET000
065000980921      *
065100980923     C                   ENDSR
065200980506      *****************************************************************
065300980506      *   ROUTINE INIZIALE
065400980506      *****************************************************************
065500980506     C     *INZSR        BEGSR
065600980506      *
065700980506     C     *ENTRY        PLIST
065800980506     C                   PARM                    KPJBA
065900980730      *
066000981001     C                   MOVEL     KPJBU         FIDN18DS
066100980921
066200980921      * DATA
066300980729     C                   TIME                    W0140
066400980507     C                   MOVE      W0140         WDTGIO
066500980921     C                   MOVEL     W0140         WORA
066600980921      *
066700980728     C                   Z-ADD     WDTGIO        G02DAT
066800980728     C                   MOVEL     *BLANK        G02ERR
066900980728     C                   CALL      'XSRDA8'
067000980728     C                   PARM                    WLBDAT
067100980728     C                   MOVEL     G02INV        DATEU
067200980730     C     *iso          MOVEL     DATEU         DATA_oggi
067300981001
067400010905      **** Aggancio tabella STD
067500010905      **** => NON serve !!!
067600010905     C***                   MOVEL     'C'           t02mod
067700010905     C***                   MOVEL     knsif         t02sif
067800010905     C***                   MOVEL     'STD'         t02cod
067900010905     C***                   MOVEL(P)  1             t02ke1
068000010905     C***                   CALL      'TIBS02R'
068100010905     C***                   PARM                    KPJBA
068200010905     C***                   PARM                    TIBS02DS
068300010905     C***                   if        t02err = *BLANKS
068400010905     C***                   MOVEL     T02UNI        DSTD
068500010905     C***                   endif
068600980921
068700980915     C     KSPE          KLIST
068800980915     C                   KFLD                    KAAS
068900980915     C                   KFLD                    KLNP
069000980915     C                   KFLD                    KNRS
069100980915     C                   KFLD                    KNSP
069200980915      *
069300980915     C     KDCT          KLIST
069400980915     C                   KFLD                    KAAC
069500980915     C                   KFLD                    KFIL
069600980915     C                   KFLD                    KNCA
069700980917      *
069800980917     C     KDCS          KLIST
069900980917     C                   KFLD                    KTPD
070000980917     C                   KFLD                    KNKT
070100980917     C                   KFLD                    KSTD
070200981001      *
070300981001     C     KDET          KLIST
070400990127     C                   KFLD                    I18AAE
070500990127     C                   KFLD                    I18NEV
070600980921
070700980729     C                   ENDSR
070800980914      *----------------------------------------------------------------
070900980923     OFNDCT000  E            UPDDCT
071000980923     O                       DCTgfc
071100980923     O                       DCTfca
071200981222     O                       DCTft1
071300981222     O                       DCTft2
071400981117     O                       DCTaad
071500980923     O                       DCTpod
071600980923     O                       DCTnde
071700980921      *----------------------------------------------------------------
071800980914      *  L E T T E R A
071900980914      *----------------------------------------------------------------
072000980914     OPRTF198   E            EXLEautor        15
072100980923     o                                           13 'Pag.'
072200980923     o                       PAGE                17
072300980923     o                                           65 'Spett.le'
072400980921     o          E            EXLEautor   1
072500980921     O                       Prag                92
072600980914     o          E            EXLEautor   1
072700980921     O                       Pind                92
072800980914     o          E            EXLEautor   1
072900980923     O                       Data_oggi           19
073000980921     O                       Pcap                57
073100980921     O                       Ploc                92
073200980921     O                       Ppro                95
073300980914      *
073400980914     o          E            EXLEspazio  1
073500980914      *
073600980914     o          E            EXLEdetlet  1
073700980914     O                       DELETT             109
073800980914      *
073900980914     o          E            EXLEfirma   2
074000980923     O                                           60 'In fede'
074100980921     o          E            EXLEfirma   3
074200981001     O                       LET(3)             109
074300980921     o          E            EXLEfirma   1
074400981001     O                       LET(4)             109
074500980921      * dettaglio C.A.
074600990212     O          E            EXLEtall         15
074700980923     o                                           13 'Pag.'
074800980923     o                       PAGE                17
074900980923     o                                           90 'Nostro riferimento denunci'
075000980923     o                                           92 'a:'
075100981117     o                       Paad                97
075200981002     o                       Ppod               101
075300980923     o                       Pnde          z    109
075400990212     O          E            EXLEtall    2
075500980921     O                       KLE(1)             109
075600990212     O          E            EXLEtall    1
075700981007     O                       KLE(3)             109
075800990212     O          E            EXLEtall    1  2
075900990223     O                       KLE(5)             109
076000980921      *
076100990212     O          E            EXLEdall01  1
076200981007     o                                           10 '-'
076300981007     o                       Plnp             +   1
076400981007     o                       Pnrs          z  +   1
076500981007     o                       Pnsp          z  +   1
076600981007     o                       Pdts          y  +   3
076700981007     o                       Pncl          z  +   7
076800981007     o                       Ppkf          3  +   7
076900981007     o                       Pnas             +   7
077000981007     o                       Paac2            +   7
077100981007     o                       Pfil             +   1
077200981007     o                       Pnca          z  +   1
077300990212     O          E            EXLEdall02  1
077400981007     o                       PLErsm              40
077500981007     o                       PLErsd           +   2
077600981007     o                       PLElod           +   2
077700981007     o                       Pprd             +   1
077800990212     O          E            EXLEdall03  1
077900990223     o                       Pdes                45
078000990223     o                       PLEdespz1          109
078100990212     O          E            EXLEdall04  1
078200990223     o                       PLEdespz2          109
078300980922     O          E            EXLEfine    1
078400980922     o                       TRAT100            109
078500980914      *----------------------------------------------------------------
078600980914      *  U S O   B O L L O
078700980914      *----------------------------------------------------------------
078800980914     OPRTF198   E            EXBOautor        11
078900980923     o                                           18 'Pag.'
079000980923     o                       PAGE                22
079100980923     o                                           63 'Spett.le'
079200980922     o          E            EXBOautor   2
079300980921     O                       Prag                90
079400980922     o          E            EXBOautor   2
079500980921     O                       Pind                90
079600980922     o          E            EXBOautor   2
079700980923     O                       Data_oggi           24
079800980921     O                       Pcap                55
079900980921     O                       Ploc                90
080000980921     O                       Ppro                93
080100980914      *
080200980914     o          E            EXBOdetbol  2
080300980914     O                       DEBOLL              93
080400980914      *
080500980914     O          E            EXBOfirma   2
080600980923     O                                           65 'In fede.'
080700980921     O          E            EXBOfirma   2
080800981001     O                       BOL(4)              93
080900980921     O          E            EXBOfirma   2
081000981001     O                       BOL(5)              93
081100980921      * dettaglio C.A.
081200990212     O          E            EXBOtall         11
081300980923     o                                           18 'Pag.'
081400980923     o                       PAGE                22
081500980923     o                                           74 'Nostro riferimento denunci'
081600980923     o                                           76 'a:'
081700981117     o                       Paad                81
081800981006     o                       Ppod                85
081900980923     o                       Pnde          z     93
082000990212     O          E            EXBOtall    2
082100980922     O                       KBO(1)              93
082200990212     O          E            EXBOtall    2
082300981007     O                       KBO(3)              93
082400990212     O          E            EXBOtall    2
082500981007     O                       KBO(5)              93
082600980921      *
082700990212     O          E            EXBOdall01  2
082800981007     o                                           15 '-'
082900981007     o                       Plnp             +   1
083000981007     o                       Pnrs          z  +   1
083100981007     o                       Pnsp          z  +   1
083200981007     o                       Pdts          y  +   2
083300981007     o                       Pncl          z  +   3
083400981007     o                       Ppkf          3  +   2
083500981007     o                       Pnas             +   3
083600981007     o                       Paac2            +   3
083700981007     o                       Pfil             +   1
083800981007     o                       Pnca          z  +   1
083900990212     O          E            EXBOdall02  2
084000981007     o                       PBOrsm              39
084100981007     o                       PBOrsd           +   2
084200981007     o                       PBOlod           +   2
084300981007     o                       Pprd             +   1
084400990212     O          E            EXBOdall03  2
084500990223     o                       Pdes                50
084600990223     o                       PBOdespz1           93
084700990212     O          E            EXBOdall04  2
084800990223     o                       PBOdespz2           93
084900980922      *
085000980922     O          E    01      EXBOfine         11
085100980922     o                       TRAT79              93
085200980922     O          E   N01      EXBOfine    2
085300980922     o                       TRAT79              93
085400980911      *----------------------------------------------------------------
085500980915** MSG                                                                       *
085600981001MANCA NUMERATORE DENUNCIA: avvisare l'EDP !!!!!!!                              1
085700980915NUMERATORE DENUNCIA non disponibile: RITENTARE NUOVAMENTE !!!                  2
085800980914** CMD
085900990119OVRPRTF PRTF198 FORMTYPE(DENUNCIA_B) SAVE(*YES)
086000990119OVRPRTF PRTF198 FORMTYPE(DENUNCIA_L) SAVE(*YES)
086100981001** COTE: 1 cod.testo; COSC: 2-4 posizione posizione ultimo elemento
086200981001L002
086300981001B003
086400981001** LET    4E/L100                                                                                  *
086500981001A seguito della denuncia presentata da xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx in data xx/xx/xxxx,
086600981001di cui alleghiamo copia, Vi inviamo l'elenco delle merci coinvolte.
086700980921Visto: della presentazione della denuncia alle ore        del
086800980921                  Ricevuta l'Ufficiale di P.G.
086900990212** KLE                                                                                             *
087000990223   Numero e Data Spedizione       Colli        Peso Kg          Natura Merce         Numero C.A.
087100981007- xxx xx xxxxxxx   xx/xx/xx       xxxxx       xxxxxx,x       xxxxxxxxxxxxxxx       xx xxx xxxxxxx
087200981007 Mittente                        Destinatario                    Località Destinatario
087300981007 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xx
087400990223 Peso/Colli/Pezzi mancanti            Descrizione
087500990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
087600990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
087700981001** BOL     5E/L79                                                             *
087800981001A seguito della denuncia presentata da xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx       1
087900981001in data xx/xx/xxxx, di cui alleghiamo copia, Vi inviamo l'elenco delle merci     2
088000981001coinvolte                                                                        3
088100981001Visto: della presentazione della denuncia alle ore        del                    4
088200981001                  Ricevuta l'Ufficiale di P.G.                                   5
088300990212** KBO                                                                        *
088400990223  Numero e Data Spedizione   Colli   Peso Kg      Natura Merce    Numero C.A.
088500981007- xxx xx xxxxxxx  xx/xx/xx   xxxxx  xxxxxx,x   xxxxxxxxxxxxxxx   xx xxx xxxxxxx
088600981007 Mittente                  Destinatario              Località Destinatario
088700981007 xxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxx xx
088800990223 Peso/Colli/Pezzi mancanti            Descrizione
088900990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
089000990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
