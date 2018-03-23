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
004200980914     D MAI             C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004300980914     D MIN             C                   CONST('abcdefghijklmnopqrstuvwxyz')
004400980922     D KMANP           C                   CONST('MANCANZA PARZIALE')
004500980922     D KMANT           C                   CONST('MANCANZA TOTALE')
004600980922     D KAMMA           C                   CONST('AMMANCO')
004700980729      *----------------------------------
004800980616     D                SDS
004900980616     D  VTCPGM                 1     10
005000980730      *----------------------------------
005100980730     D WLBDAT          DS                  INZ
005200980730     D  g02dat                 1      8  0
005300980730     D  g02inv                 9     16  0
005400980730     D  g02err                17     17
005500980730     D  g02tgi                18     22  0
005600980914      *
005700980914     D WDATAalfa       DS                  INZ
005800980914     D  WAAalfa                1      4
005900980914     D  WMMalfa                5      6
006000980914     D  WGGalfa                7      8
006100980914      *
006200980914     D WORAalfa        DS                  INZ
006300980914     D  WoraHH                 1      2
006400980914     D  WoraMM                 3      4
006500980915      *
006600980915     D PARAM4          DS
006700980915     D  PA4CNU                 1      3  0
006800980917      *
006900980917     D WNUMCA          DS
007000980917     D  V2HAAC
007100980917     D  V2CFIL
007200980917     D  V2CNCA
007300980922      *
007400980922     D PRTFBK          DS
007500980922     D  cur_line             367    368I 0
007600980915      *----------------------------------
007700980921     D FIDN05DS      E DS
007800990205     D FIDN18DS      E DS
007900990205     D FIDN20DS      E DS
008000980507     D KPJBA         E DS
008100010905     D*** DSTD          E DS
008200981229     D DDCT01        E DS
008300980915     D UT§DSE0F      E DS
008400980915     D TIBS02DS      E DS
008500981001     D DS_FNDFA      E DS                  EXTNAME(FNDFA00F)
008600981001     D DS_TNTMD      E DS                  EXTNAME(TNTMD00F)
008700990806     D DS_TITAS      E DS                  EXTNAME(TITAS00F)
008800980729      *----------------------------------
008900980922     D delett          S            100
009000980922     D deboll          S             79
009100980922     D trat100         S            100    INZ(*all'-')
009200980922     D trat79          S             79    INZ(*all'-')
009300980921     D command         S             80
009400980917     D w005a           S              5
009500980917     D w007a           S              7
009600980917     D w007b           S              7
009700990223     D wpzman          S             11
009800990223     D wcoman          S             11
009900990223     D wkgman          S             11
010000980915     D wsocie          S             20
010100990212     D wprimaca        S              1
010200980923     D wmaxcapag       s              2  0
010300980923     D wcaprt          s              2  0
010400980917     D XX              s              3  0
010500981001     D YY              s              3  0
010600980730     D dateu           s              8  0
010700980730     D wdtgio          S              8  0
010800980921     D w0080           S              8  0
010900980921     D wora            S              4  0
011000980730     D w0140           S             14  0
011100980915     D nrr1            S              4  0
011200980915     D nrr2            S              4  0
011300990119     D lung            S             15  5 inz(80)
011400980916     D DATA_oggi       S               D   DATFMT(*eur)
011500980917     D DATA_eur        S               D   DATFMT(*eur)
011600980730      *
011700980917     D kaas            S                   LIKE(DCTaas)
011800980917     D kfil            S                   LIKE(DCTfil)
011900980917     D klnp            S                   LIKE(DCTlnp)
012000981001     D kaac            S                   LIKE(DCTaac)
012100980917     D knca            S                   LIKE(DCTnca)
012200980917     D knrs            S                   LIKE(DCTnrs)
012300980917     D knsp            S                   LIKE(DCTnsp)
012400980917     D ktpd            S                   LIKE(DCStpd)
012500980917     D knkt            S                   LIKE(DCSnkt)
012600980917     D kstd            S                   LIKE(DCSstd)
012700980921      *
012800981117     D Paad            S                   LIKE(DCTaad)
012900980921     D Ppod            S                   LIKE(DCTpod)
013000980921     D Pnde            S                   LIKE(DCTnde)
013100981007     D Paac2           S              2S 0
013200981007     D Pfil            S                   LIKE(DCTfil)
013300981007     D Pnca            S                   LIKE(DCTnca)
013400990223     D Prag            S                   LIKE(V1aurs)
013500990223     D Pind            S                   LIKE(V1auin)
013600990223     D Pcap            S                   LIKE(V1auca)
013700990223     D Ploc            S                   LIKE(V1aulo)
013800990223     D Ppro            S                   LIKE(V1aupr)
013900980921     D Plnp            S                   LIKE(DCTlnp)
014000980921     D Pnrs            S                   LIKE(DCTnrs)
014100980921     D Pnsp            S                   LIKE(DCTnsp)
014200980921     D Pdts            S              6  0
014300981001     D Pncl            S                   LIKE(TASncl)
014400990806     D Ppkf            S                   LIKE(TASpkF)
014500981001     D Pprd            S                   LIKE(TMDprd)
014600981001     D Pnas            S                   LIKE(DCTnas)
014700981007     D PLErsm          S             30
014800981007     D PLErsd          S             30
014900981007     D PLElod          S             30
015000981007     D PBOrsm          S             24
015100981007     D PBOrsd          S             24
015200981007     D PBOlod          S             23
015300990223     D Pdes            S             35
015400990223     D PLEdespz2       S             99
015500990223     D PBOdespz2       S             78
015600990212
015700990223     d DS_DESPZ        DS
015800040330     ***D W2300A                      2300
015900040330     ***D DPM                           35    DIM(60)  overlay(W2300A:1)        Descr. Pezzi Mancan.
016000040330     ***d PLEdespz1                     62             overlay(W2300A:1)
016100040330     ***d LEdepz                        99    dim(21)  overlay(W2300A:63)
016200040330     ***d PBOdespz1                     41             overlay(W2300A:1)
016300040330     ***d BOdepz                        78    dim(27)  overlay(W2300A:42)
016400040330     d W2300A                     32600
016500040330     d  DPM                          35    DIM(930) overlay(W2300A:1)           Descr. Pezzi Mancan.
016600040330     d  PLEdespz1                    62             overlay(W2300A:1)
016700040330     d  LEdepz                       99    dim(328) overlay(W2300A:63)
016800040330     d  PBOdespz1                    41             overlay(W2300A:1)
016900040330     d  BOdepz                       78    dim(417) overlay(W2300A:42)
017000980921
017100980731      *---------------------------------------------------------------------
017200980910
017300990223      * dati autorità
017400980911     c                   EXSR      RIEdatiau
017500990127      *
017600990127      * Aggancio FNDET e imposto N. denuncia
017700990127     c     KDET          CHAIN     FNDET000                           30
017800990127     C                   z-add     DETaad        Paad
017900990127     C                   z-add     DETpod        Ppod
018000990127     C                   z-add     DETnde        Pnde
018100980914
018200980923      * Imposto tipo modulo e massimo numero c.a. per pagina per dettaglio denuncia
018300990223     c                   if        V1mode = 'B'
018400980914     c                   movea     CMD(1)        COMMAND
018500980923     c                   z-add     7             wmaxcapag
018600980914     c                   else
018700980917     c                   movea     CMD(2)        COMMAND
018800980923     c                   z-add     11            wmaxcapag
018900980914     c                   endif
019000980923      *
019100980914     c                   call      'QCMDEXC'
019200980914     c                   parm                    COMMAND
019300980914     c                   parm                    LUNG
019400980914
019500980914      * Apro file stampa testo denuncia
019600980914     c                   OPEN      PRTF198
019700980914
019800981001     c                   z-add     1             YY
019900990223     c     V1mode        lookup    COTE(YY)                               30
020000981001
020100990223     c                   if        V1mode = 'L'
020200980911      * Stampo testo denuncia tipo lettera
020300980911     c                   EXSR      PRTdelet
020400980911     c                   else
020500980911      * Stampo testo denuncia tipo uso bollo
020600980911     c                   EXSR      PRTdebol
020700980911     c                   endif
020800980914
020900980910      * Stampo allegato
021000980915     c                   EXSR      PRTalleg
021100980922
021200980922      * Chiudo file stampa testo denuncia
021300980922     c                   CLOSE     PRTF198
021400980910
021500981029     c                   clear                   tibs02ds
021600981029     c                   movel     'C'           t02tla
021700981029     c                   call      'TIBS02R'
021800981029     c                   parm                    kpjba
021900981029     c                   parm                    tibs02ds
022000980506     C                   EVAL      *INLR = *ON
022100980911      *****************************************************************
022200980911      *   IMPOSTO DATI AUTORITA'
022300980911      *****************************************************************
022400980911     C     RIEdatiau     BEGSR
022500990223      *
022600990223     c                   z-add     1             nrr2
022700990223     c     NRR2          CHAIN     FI18S02                            32
022800990223      *
022900990223     c                   movel     V1aurs        Prag
023000990223     c                   movel     V1auin        Pind
023100990223     c                   movel     V1auca        Pcap
023200990223     c                   movel     V1aulo        Ploc
023300990223     c                   movel     V1aupr        Ppro
023400980911      *
023500980911     c                   ENDSR
023600980729      *****************************************************************
023700980911      *   STAMPO TESTO DENUNCIA MODULO LETTERA
023800980729      *****************************************************************
023900980911     C     PRTdelet      BEGSR
024000980730
024100980914      *  Stampo autorità
024200980914     C                   EXCEPT    EXLEautor
024300980914
024400980914      *  Spazio dall'autorità al testo
024500980914     c                   DO        4             XX
024600980914     C                   EXCEPT    EXLEspazio
024700980914     c                   ENDDO
024800980731
024900980914      *  Stampo testo
025000981001     c                   DO        COSC(YY)      XX
025100980921     c                   MOVEL(P)  LET(XX)       delett
025200980914     c                   EXSR      impvarlet
025300980914     C                   EXCEPT    EXLEdetlet
025400980914     c                   ENDDO
025500980914
025600980914      *  Stampa firma
025700980914     C                   EXCEPT    EXLEfirma
025800980914
025900980730     C                   ENDSR
026000980804      *****************************************************************
026100980914      *   IMPOSTO VARIABILI TESTO DENUNCIA  ** LETTERA **
026200980804      *****************************************************************
026300980914     C     IMPVARLET     BEGSR
026400980804      *
026500981001     c                   IF        XX = 1
026600981001     c                   eval      %subst(delett:40:35) = I18prd
026700981001     c                   movel     I18dpr        Wdataalfa
026800981001     c                   eval      %subst(delett:84:10)  =
026900980914     c                                 (WGGalfa + '/' + WMMalfa + '/' + WAAalfa)
027000981001     c                   ENDIF
027100980914      *
027200980914     C                   ENDSR
027300980914      *****************************************************************
027400980914      *   STAMPO TESTO DENUNCIA MODULO USO BOLLO
027500980914      *****************************************************************
027600980914     C     PRTdebol      BEGSR
027700980914
027800980914      *  Stampo autorità
027900980914     C                   EXCEPT    EXBOautor
028000980914
028100980914      *  Stampo testo
028200981001     c                   DO        COSC(YY)      XX
028300980921     c                   MOVEL(P)  BOL(XX)       deboll
028400980914     c                   EXSR      impvarbol
028500980914     C                   EXCEPT    EXBOdetbol
028600980914     c                   ENDDO
028700980914
028800980914      *  Stampa firma
028900980915     C                   EXCEPT    EXBOfirma
029000980914
029100980914     C                   ENDSR
029200980914      *****************************************************************
029300980914      *   IMPOSTO VARIABILI TESTO DENUNCIA  ** BOLLO **
029400980914      *****************************************************************
029500980914     C     IMPVARBOL     BEGSR
029600980914      *
029700981117     c                   IF        XX = 1
029800981001     c                   eval      %subst(deboll:40:35) = I18prd
029900981117     c                   ENDIF
030000980914      *
030100981117     c                   IF        XX = 2
030200981001     c                   movel     I18dpr        Wdataalfa
030300981001     c                   eval      %subst(deboll:9:10)  =
030400981001     c                                 (WGGalfa + '/' + WMMalfa + '/' + WAAalfa)
030500981117     c                   ENDIF
030600980914      *
030700980914     C                   ENDSR
030800980915      *****************************************************************
030900980915      *   STAMPO ALLEGATO
031000980915      *****************************************************************
031100980915     C     PRTALLEG      BEGSR
031200980915      *
031300980915     c                   z-add     1             nrr2
031400980915      *
031500981001     c     NRR2          CHAIN     FI18S02                            32
031600980915      *
031700980915     c     *IN32         doweq     *OFF
031800980915      *
031900981001      * Elaboro se in fase corretta e scelta non bloccata
032000990223     c                   IF        V2HI59 <> '1'  and  V2HI58 <> '1'
032100980915      *
032200990127      * Se non ancora impostato il n° denuncia lo ricavo e aggiorno FNDET
032300990127     c                   if        Pnde = *zeros
032400980915     c                   EXSR      NUMDEN
032500980923     c                   EXSR      AGGFNDET
032600980915     c                   endif
032700980921      *
032800980922      * Imposto dati righe dettaglio
032900980917     c                   EXSR      RIEDET
033000990212      *
033100990212      * Per la prima CA salto a pagina nuova
033200990212     c                   IF           Wprimaca = *blanks
033300990212     c                   eval      Wprimaca = 'S'
033400990223     c                   If        V1mode = 'L'
033500990212     c                   except    EXLEtall
033600990212     c                   Else
033700990212     c                   except    EXBOtall
033800990212     c                   Endif
033900990212     c                   ENDIF
034000990212      *
034100980917      * Stampo
034200990212     c                   exsr      PRTca
034300980921      *
034400980921      * Aggiorno files
034500040330     c                   if        KNMUS <> *all'1'
034600980923     c                   EXSR      AGGFNDCT
034700980923     c                   EXSR      AGGFNDCF
034800040330     c                   endif
034900980917      *
035000980915     c                   ENDIF
035100980915      *
035200980915     c                   add       1             nrr2
035300981001     c     NRR2          CHAIN     FI18S02                            32
035400980915     c                   enddo
035500980915      *
035600980922      * Stampo tratteggio di chiusura
035700990223     c                   If        V1mode = 'L'
035800980922     c                   EXCEPT    EXLEfine
035900980922     c                   else
036000980922     C  N01              eval      *in01 = (CUR_line = 59)
036100980922     c                   EXCEPT    EXBOfine
036200980922     c                   endif
036300980922      *
036400980915     C                   ENDSR
036500980915      *****************************************************************
036600980915      *   STACCO NUMERO DENUNCIA
036700980915      *****************************************************************
036800980915     C     NUMDEN        BEGSR
036900990205
037000990205      * Il controllo dell'esistenza del numeratore è effettuato dal PGM di lancio
037100990205
037200990205     C                   clear                   FIDN20DS
037300990205     C                   movel     'N'           I20mod
037400990205     C                   z-add     DETaae        I20pra
037500990205     C                   movel     'DEN'         I20dit
037600990205      *
037700990205     C                   movel(P)  FIDN20DS      KPJBU
037800990205      *
037900990205     c                   CALL      'FIDN20R'
038000990205     c                   PARM                    KPJBA
038100990205      *
038200990205     C                   movel     KPJBU         FIDN20DS
038300990205      *
038400990205     c                   IF          O20err <> 'E'
038500990205     C                   z-add     DETaae        PAAD
038600990205     C                   Z-ADD     I18FGS        Ppod
038700990205     C                   Z-ADD     O20prn        Pnde
038800990205     C                   ENDIF
038900990205
039000980915     C                   ENDSR
039100980915      *****************************************************************
039200980916      *   RIEMPIO I CAMPI DEL DETTAGLIO
039300980915      *****************************************************************
039400980917     C     RIEDET        BEGSR
039500980917      *
039600980915      * Dati spedizione
039700980915     c                   z-add     V2Haas        Kaas
039800980915     c                   z-add     V2Clnp        Klnp
039900980915     c                   z-add     V2Cnrs        Knrs
040000980915     c                   z-add     V2Cnsp        Knsp
040100980915      *
040200990806     c                   clear                   DS_TITAS
040300990806     c     KSPE          chain     TITAS30C                           30
040400981001     c                   clear                   DS_tntmd
040500981001     c     KSPE          chain     TNTMD000                           30
040600981001      *
040700981001     c                   Z-ADD     V2Clnp        Plnp
040800981001     c                   Z-ADD     V2Cnrs        Pnrs
040900981001     c                   Z-ADD     V2Cnsp        Pnsp
041000981001     c                   z-add     V2Haas        w0080
041100981001     c                   movel     TASmgs        w0080
041200981001     c                   if        TASmgs > *zeros
041300981001     c     *USA          move      w0080         DATA_eur
041400981001     c     *dmy          MOVE      DATA_eur      Pdts
041500981001     c                   endif
041600981001     c                   Z-ADD     TASncl        Pncl
041700990806     c                   Z-ADD     TASpkF        Ppkf
041800981001     c                   MOVEL     TMDprd        Pprd
041900980921      *
042000990223     c                   If        V1mode = 'L'
042100981001     c                   MOVEL     TMDrsm        PLErsm
042200981001     c                   MOVEL     TMDrsd        PLErsd
042300981001     c                   MOVEL     TMDlod        PLElod
042400980921     c                   else
042500981001     c                   MOVEL     TMDrsm        PBOrsm
042600981001     c                   MOVEL     TMDrsd        PBOrsd
042700981001     c                   MOVEL     TMDlod        PBOlod
042800980921     c                   endif
042900980917      *
043000980915      * Dati C.A.
043100980917     c                   z-add     V2Haac        Kaac
043200980917     c                   z-add     V2Cfil        Kfil
043300980917     c                   z-add     V2Cnca        Knca
043400980915     C     kdct          CHAIN     fndct000                           31
043500981229     c                   movel     DCTflo        DDCT01
043600981229     c                   movel     DCTnas        Pnas
043700981007     c                   z-add     V2Haac        Paac2
043800981007     c                   z-add     V2Cfil        Pfil
043900981007     c                   z-add     V2Cnca        Pnca
044000980917      *
044100981001      * Reperisco dati per descrizione danno
044200990223     c                   movel     *blanks       wpzman
044300981001     c                   movel     *blanks       wcoman
044400981001     c                   movel     *blanks       wkgman
044500990223     C                   clear                   Pdes
044600980917     c                   clear                   XX
044700980917      *
044800981001      * Kg danneggiati
044900981117     c                   IF        DCTpkd > *zeros
045000980917     c                   move(P)   DCTpkd        w007a
045100980917     c     '0'           check     w007a         xx
045200980917     c                   eval      W007b = %subst(w007a:xx)
045300980922     c     ' '           checkr    w007b         xx
045400980917     c                   if        xx = 1
045500981104     c                   eval      Wkgman = 'Kg 0,'  +  %subst(w007b:1:1)
045600981104     c                   else
045700981104     c                   eval      Wkgman =  'Kg '  +  %subst(w007b:1:(xx-1))
045800981104     c                                      +  ','  +  %subst(w007b:XX:1)
045900980917     c                   endif
046000980917     c                   ENDIF
046100980917      *
046200981001      * Colli danneggiati
046300990223     c                   IF        DCTncn > *zeros
046400980917     c                   move      DCTncn        w005a
046500980917     c     '0'           check     w005a         xx
046600990223     c                   eval      Wcoman = 'Colli '  +  %subst(W005a:XX)
046700990223     c                   ENDIF
046800980917      *
046900981001      * Pezzi danneggiati
047000990223     c                   IF        DCTnpz > *zeros
047100980917     c                   move      DCTnpz        w005a
047200980917     c     '0'           check     w005a         xx
047300990223     c                   eval      Wpzman = 'Pezzi '  +  %subst(W005a:XX)
047400990223     c                   ENDIF
047500980917      *
047600990223     c                   eval      Pdes =     %trim(WKGman)  +  '  '
047700990223     c                                     +  %trim(WCOman)  +  '  '
047800990223     c                                     +  %trim(WPZman)
047900990212      *
048000990223      * Descrizione pezzi mancanti
048100990309     c                   clear                   W2300A
048200990223     c                   IF          DCTnpz > *zeros
048300990212     C                   movel(P)  'C'           Ktpd
048400990212     C                   movel(P)  Wnumca        Knkt
048500990223     C                   movel(P)  'P'           Kstd
048600990212     c                   clear                   XX
048700990212      *
048800990212     c     kdcs          chain     fndcs000                           30
048900990212      *
049000040330     ***c     *in30         DOWeq     *off
049100040330     ***c     xx            ANDlt     60
049200040330     c                   DOW           *in30 = *off
049300040330     c                             and xx    < %elem(DPM)
049400990212     c                   add       1             XX
049500990223     c                   movel     dcsnot        DPM(XX)
049600990212     c     kdcs          reade     fndcs000                               30
049700990212     c                   ENDDO
049800990223      *
049900990223     c                   ENDIF
050000980917      *
050100980915     C                   ENDSR
050200990212      *****************************************************************
050300990212      *   STAMPA CA IN DENUNCIA
050400990212      *****************************************************************
050500990212     C     PRTca         BEGSR
050600990212      *
050700990212     c                   exsr      PRTsalto
050800990223     c                   If        V1mode = 'L'
050900990212     c                   except    EXLEdall01
051000990212     c                   Else
051100990212     c                   except    EXBOdall01
051200990212     c                   Endif
051300990212      *
051400990212     c                   exsr      PRTsalto
051500990223     c                   If        V1mode = 'L'
051600990212     c                   except    EXLEdall02
051700990212     c                   Else
051800990212     c                   except    EXBOdall02
051900990212     c                   Endif
052000990212      *
052100990212     c                   exsr      PRTsalto
052200990223     c                   If        V1mode = 'L'
052300990212     c                   except    EXLEdall03
052400990212     c                   Else
052500990212     c                   except    EXBOdall03
052600990212     c                   Endif
052700990212      *
052800990223     c                   If        V1mode = 'L'
052900990223     c                   clear                   PLEdespz2
053000040330     ***c                   DO        21            XX
053100040330     c                   clear                   xx
053200040330     c                   DOw       XX < %elem(LEdepz)
053300040330     c                   add       1             xx
053400990223     c                   If        LEdepz(XX) <> *blanks
053500990223     c                   movel     LEdepz(XX)    PLEdespz2
053600990212     c                   exsr      PRTsalto
053700990212     c                   except    EXLEdall04
053800990212     c                   Endif
053900990212     c                   ENDDO
054000990212      *
054100990212     c                   ELSE
054200990223     c                   clear                   PBOdespz2
054300040330     ***c                   DO        27            XX
054400040330     c                   clear                   xx
054500040330     c                   DOw       XX < %elem(BOdepz)
054600040330     c                   add       1             xx
054700990223     c                   If        BOdepz(XX) <> *blanks
054800990223     c                   movel     BOdepz(XX)    PBOdespz2
054900990212     c                   exsr      PRTsalto
055000990212     c                   except    EXBOdall04
055100990212     c                   Endif
055200990212     c                   ENDDO
055300990212     c                   ENDIF
055400990212      *
055500990212     C                   ENDSR
055600990212      *****************************************************************
055700990212      *   SALTO PAGINA
055800990212      *****************************************************************
055900990212     C     PRTSALTO      BEGSR
056000990212      *
056100990212     c                   SELECT
056200990212     c                   WHEN      CUR_line <  59
056300990223     c                   WHEN      V1mode = 'L'
056400990212     c                   except    EXLEtall
056500990223     c                   WHEN      V1mode = 'B'
056600990212     c                   except    EXBOtall
056700990212     c                   ENDSL
056800990212      *
056900990212     C                   ENDSR
057000980922      *****************************************************************
057100980922      *   SCRIVO FNDCF FASI C.A.
057200980922      *****************************************************************
057300980923     C     AGGFNDCF      BEGSR
057400980923      *
057500980923     C                   z-add     DCTaaC        DCFaac
057600980923     C                   z-add     DCTfil        DCFfil
057700980923     C                   z-add     DCTnca        DCFnca
057800981001     c                   z-add     I18fca        DCFfca
057900980923     c                   z-add     dateu         DCFdfc
058000980923     c                   z-add     wora          DCFhfc
058100981001     c                   z-add     I18fgs        DCFfev
058200980923     c                   movel     knmus         DCFpru
058300980923      *
058400980923     c                   WRITE     FNDCF000
058500980923      *
058600980923     C                   ENDSR
058700980921      *****************************************************************
058800980921      *   AGGIORNO FNDCT TESTATA C.A.
058900980921      *****************************************************************
059000980923     C     AGGFNDCT      BEGSR
059100980921      *
059200980921      *  Richiamo pgm per determinare il P.O. che gestirà la prossima fase
059300980923     c                   CLEAR                   FIDN05DS
059400980923     c                   movel     'F'           I05MOD
059500981001     c                   z-add     I18fca        I05fca
059600980923     c                   movel     'P'           I05fpr
059700980923     c                   movel     DCTptr        I05tpc
059800980923     c                   movel     'E'           I05fpe
059900981030     c                   movel     'D'           I05fde
060000980923     c                   movel     'O'           I05ffs
060100980923     c                   z-add     dateu         i05dta
060200980923     c                   move      DCTtad        i05tad
060300981125      * Valorizzo numero CA
060400981125     c                   z-add     DCTaac        i05aac
060500981125     c                   z-add     DCTfil        i05fil
060600981125     c                   z-add     DCTnca        i05nca
060700980921
060800980923     C                   CALL      'FIDN05R'
060900980923     c                   PARM                    KPJBA
061000980923     c                   PARM                    FIDN05DS
061100980921
061200980923     c                   IF        O05err = *blanks
061300980923     c                   movel     O05REC        DS_FNDFA
061400980923     c                   SELECT
061500980923     c                   WHEN      DFAgfs = 'P'
061600981229     c                   movel     §DCTlnpc      dctgfc
061700980923     c                   WHEN      DFAgfs = 'S'
061800980923     c                   z-add     46            dctgfc
061900981023     c                   WHEN      DFAgfs = 'A'
062000981023     c                   z-add     DCTlna        dctgfc
062100980923     c                   OTHER
062200981023     c                   z-add     DCTfil        dctgfc
062300980923     c                   ENDSL
062400980923     c                   ENDIF
062500980921      *
062600980921      * Imposto campi per trasmissione dati
062700981001     c                   z-add     I18fca        DCTfca
062800981117     C                   z-add     Paad          DCTaad
062900980923     C                   z-add     Ppod          DCTpod
063000980923     C                   z-add     Pnde          DCTnde
063100981222     c                   clear                   DCTft1
063200981222     c                   clear                   DCTft2
063300980921      *
063400980923     c                   EXCEPT    UPDdct
063500980921      *
063600980923     C                   ENDSR
063700980921      *****************************************************************
063800981001      *   AGGIORNO FNDET TESTATA DENUNCIA
063900980921      *****************************************************************
064000980923     C     AGGFNDET      BEGSR
064100981001      *
064200981117     C                   z-add     Paad          DETaad
064300980923     C                   z-add     Ppod          DETpod
064400981117     C                   z-add     Pnde          DETnde
064500980923     c                   z-add     dateu         DETdsb
064600980921      *
064700981001     c                   UPDATE    FNDET000
064800980921      *
064900980923     C                   ENDSR
065000980506      *****************************************************************
065100980506      *   ROUTINE INIZIALE
065200980506      *****************************************************************
065300980506     C     *INZSR        BEGSR
065400980506      *
065500980506     C     *ENTRY        PLIST
065600980506     C                   PARM                    KPJBA
065700980730      *
065800981001     C                   MOVEL     KPJBU         FIDN18DS
065900980921
066000980921      * DATA
066100980729     C                   TIME                    W0140
066200980507     C                   MOVE      W0140         WDTGIO
066300980921     C                   MOVEL     W0140         WORA
066400980921      *
066500980728     C                   Z-ADD     WDTGIO        G02DAT
066600980728     C                   MOVEL     *BLANK        G02ERR
066700980728     C                   CALL      'XSRDA8'
066800980728     C                   PARM                    WLBDAT
066900980728     C                   MOVEL     G02INV        DATEU
067000980730     C     *iso          MOVEL     DATEU         DATA_oggi
067100981001
067200010905      **** Aggancio tabella STD
067300010905      **** => NON serve !!!
067400010905     C***                   MOVEL     'C'           t02mod
067500010905     C***                   MOVEL     knsif         t02sif
067600010905     C***                   MOVEL     'STD'         t02cod
067700010905     C***                   MOVEL(P)  1             t02ke1
067800010905     C***                   CALL      'TIBS02R'
067900010905     C***                   PARM                    KPJBA
068000010905     C***                   PARM                    TIBS02DS
068100010905     C***                   if        t02err = *BLANKS
068200010905     C***                   MOVEL     T02UNI        DSTD
068300010905     C***                   endif
068400980921
068500980915     C     KSPE          KLIST
068600980915     C                   KFLD                    KAAS
068700980915     C                   KFLD                    KLNP
068800980915     C                   KFLD                    KNRS
068900980915     C                   KFLD                    KNSP
069000980915      *
069100980915     C     KDCT          KLIST
069200980915     C                   KFLD                    KAAC
069300980915     C                   KFLD                    KFIL
069400980915     C                   KFLD                    KNCA
069500980917      *
069600980917     C     KDCS          KLIST
069700980917     C                   KFLD                    KTPD
069800980917     C                   KFLD                    KNKT
069900980917     C                   KFLD                    KSTD
070000981001      *
070100981001     C     KDET          KLIST
070200990127     C                   KFLD                    I18AAE
070300990127     C                   KFLD                    I18NEV
070400980921
070500980729     C                   ENDSR
070600980914      *----------------------------------------------------------------
070700980923     OFNDCT000  E            UPDDCT
070800980923     O                       DCTgfc
070900980923     O                       DCTfca
071000981222     O                       DCTft1
071100981222     O                       DCTft2
071200981117     O                       DCTaad
071300980923     O                       DCTpod
071400980923     O                       DCTnde
071500980921      *----------------------------------------------------------------
071600980914      *  L E T T E R A
071700980914      *----------------------------------------------------------------
071800980914     OPRTF198   E            EXLEautor        15
071900980923     o                                           13 'Pag.'
072000980923     o                       PAGE                17
072100980923     o                                           65 'Spett.le'
072200980921     o          E            EXLEautor   1
072300980921     O                       Prag                92
072400980914     o          E            EXLEautor   1
072500980921     O                       Pind                92
072600980914     o          E            EXLEautor   1
072700980923     O                       Data_oggi           19
072800980921     O                       Pcap                57
072900980921     O                       Ploc                92
073000980921     O                       Ppro                95
073100980914      *
073200980914     o          E            EXLEspazio  1
073300980914      *
073400980914     o          E            EXLEdetlet  1
073500980914     O                       DELETT             109
073600980914      *
073700980914     o          E            EXLEfirma   2
073800980923     O                                           60 'In fede'
073900980921     o          E            EXLEfirma   3
074000981001     O                       LET(3)             109
074100980921     o          E            EXLEfirma   1
074200981001     O                       LET(4)             109
074300980921      * dettaglio C.A.
074400990212     O          E            EXLEtall         15
074500980923     o                                           13 'Pag.'
074600980923     o                       PAGE                17
074700980923     o                                           90 'Nostro riferimento denunci'
074800980923     o                                           92 'a:'
074900981117     o                       Paad                97
075000981002     o                       Ppod               101
075100980923     o                       Pnde          z    109
075200990212     O          E            EXLEtall    2
075300980921     O                       KLE(1)             109
075400990212     O          E            EXLEtall    1
075500981007     O                       KLE(3)             109
075600990212     O          E            EXLEtall    1  2
075700990223     O                       KLE(5)             109
075800980921      *
075900990212     O          E            EXLEdall01  1
076000981007     o                                           10 '-'
076100981007     o                       Plnp             +   1
076200981007     o                       Pnrs          z  +   1
076300981007     o                       Pnsp          z  +   1
076400981007     o                       Pdts          y  +   3
076500981007     o                       Pncl          z  +   7
076600981007     o                       Ppkf          3  +   7
076700981007     o                       Pnas             +   7
076800981007     o                       Paac2            +   7
076900981007     o                       Pfil             +   1
077000981007     o                       Pnca          z  +   1
077100990212     O          E            EXLEdall02  1
077200981007     o                       PLErsm              40
077300981007     o                       PLErsd           +   2
077400981007     o                       PLElod           +   2
077500981007     o                       Pprd             +   1
077600990212     O          E            EXLEdall03  1
077700990223     o                       Pdes                45
077800990223     o                       PLEdespz1          109
077900990212     O          E            EXLEdall04  1
078000990223     o                       PLEdespz2          109
078100980922     O          E            EXLEfine    1
078200980922     o                       TRAT100            109
078300980914      *----------------------------------------------------------------
078400980914      *  U S O   B O L L O
078500980914      *----------------------------------------------------------------
078600980914     OPRTF198   E            EXBOautor        11
078700980923     o                                           18 'Pag.'
078800980923     o                       PAGE                22
078900980923     o                                           63 'Spett.le'
079000980922     o          E            EXBOautor   2
079100980921     O                       Prag                90
079200980922     o          E            EXBOautor   2
079300980921     O                       Pind                90
079400980922     o          E            EXBOautor   2
079500980923     O                       Data_oggi           24
079600980921     O                       Pcap                55
079700980921     O                       Ploc                90
079800980921     O                       Ppro                93
079900980914      *
080000980914     o          E            EXBOdetbol  2
080100980914     O                       DEBOLL              93
080200980914      *
080300980914     O          E            EXBOfirma   2
080400980923     O                                           65 'In fede.'
080500980921     O          E            EXBOfirma   2
080600981001     O                       BOL(4)              93
080700980921     O          E            EXBOfirma   2
080800981001     O                       BOL(5)              93
080900980921      * dettaglio C.A.
081000990212     O          E            EXBOtall         11
081100980923     o                                           18 'Pag.'
081200980923     o                       PAGE                22
081300980923     o                                           74 'Nostro riferimento denunci'
081400980923     o                                           76 'a:'
081500981117     o                       Paad                81
081600981006     o                       Ppod                85
081700980923     o                       Pnde          z     93
081800990212     O          E            EXBOtall    2
081900980922     O                       KBO(1)              93
082000990212     O          E            EXBOtall    2
082100981007     O                       KBO(3)              93
082200990212     O          E            EXBOtall    2
082300981007     O                       KBO(5)              93
082400980921      *
082500990212     O          E            EXBOdall01  2
082600981007     o                                           15 '-'
082700981007     o                       Plnp             +   1
082800981007     o                       Pnrs          z  +   1
082900981007     o                       Pnsp          z  +   1
083000981007     o                       Pdts          y  +   2
083100981007     o                       Pncl          z  +   3
083200981007     o                       Ppkf          3  +   2
083300981007     o                       Pnas             +   3
083400981007     o                       Paac2            +   3
083500981007     o                       Pfil             +   1
083600981007     o                       Pnca          z  +   1
083700990212     O          E            EXBOdall02  2
083800981007     o                       PBOrsm              39
083900981007     o                       PBOrsd           +   2
084000981007     o                       PBOlod           +   2
084100981007     o                       Pprd             +   1
084200990212     O          E            EXBOdall03  2
084300990223     o                       Pdes                50
084400990223     o                       PBOdespz1           93
084500990212     O          E            EXBOdall04  2
084600990223     o                       PBOdespz2           93
084700980922      *
084800980922     O          E    01      EXBOfine         11
084900980922     o                       TRAT79              93
085000980922     O          E   N01      EXBOfine    2
085100980922     o                       TRAT79              93
085200980911      *----------------------------------------------------------------
085300980915** MSG                                                                       *
085400981001MANCA NUMERATORE DENUNCIA: avvisare l'EDP !!!!!!!                              1
085500980915NUMERATORE DENUNCIA non disponibile: RITENTARE NUOVAMENTE !!!                  2
085600980914** CMD
085700990119OVRPRTF PRTF198 FORMTYPE(DENUNCIA_B) SAVE(*YES)
085800990119OVRPRTF PRTF198 FORMTYPE(DENUNCIA_L) SAVE(*YES)
085900981001** COTE: 1 cod.testo; COSC: 2-4 posizione posizione ultimo elemento
086000981001L002
086100981001B003
086200981001** LET    4E/L100                                                                                  *
086300981001A seguito della denuncia presentata da xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx in data xx/xx/xxxx,
086400981001di cui alleghiamo copia, Vi inviamo l'elenco delle merci coinvolte.
086500980921Visto: della presentazione della denuncia alle ore        del
086600980921                  Ricevuta l'Ufficiale di P.G.
086700990212** KLE                                                                                             *
086800990223   Numero e Data Spedizione       Colli        Peso Kg          Natura Merce         Numero C.A.
086900981007- xxx xx xxxxxxx   xx/xx/xx       xxxxx       xxxxxx,x       xxxxxxxxxxxxxxx       xx xxx xxxxxxx
087000981007 Mittente                        Destinatario                    Località Destinatario
087100981007 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xx
087200990223 Peso/Colli/Pezzi mancanti            Descrizione
087300990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
087400990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
087500981001** BOL     5E/L79                                                             *
087600981001A seguito della denuncia presentata da xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx       1
087700981001in data xx/xx/xxxx, di cui alleghiamo copia, Vi inviamo l'elenco delle merci     2
087800981001coinvolte                                                                        3
087900981001Visto: della presentazione della denuncia alle ore        del                    4
088000981001                  Ricevuta l'Ufficiale di P.G.                                   5
088100990212** KBO                                                                        *
088200990223  Numero e Data Spedizione   Colli   Peso Kg      Natura Merce    Numero C.A.
088300981007- xxx xx xxxxxxx  xx/xx/xx   xxxxx  xxxxxx,x   xxxxxxxxxxxxxxx   xx xxx xxxxxxx
088400981007 Mittente                  Destinatario              Località Destinatario
088500981007 xxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxx xx
088600990223 Peso/Colli/Pezzi mancanti            Descrizione
088700990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
088800990223 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
