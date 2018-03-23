000100971203     H*------------------------------------------------------------------------*
000200150324     H* RICERCA CODICI autisti senza nazionalità
000300971203     H*------------------------------------------------------------------------*
000400971203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500971203     F*------------------------------------------------------------------------*
000600971203     F* DATA BASE
000700971203     F*------------------------------------------------------------------------*
000800150324     Faitra03l  IF   E           K DISK    infds(trads)
000900150325     Faitra00F  uF   E             DISK    rename(aitra000:aitraFIS)
001000150324     Ftrmz67d   CF   E             WORKSTN
001100971203     F                                     SFILE(SD24DSF:REC1)
001200020531     d*--------------------------------------------------------------------------------------------*
001300020531     d* Data structure                                                                             *
001400020531     d*--------------------------------------------------------------------------------------------*
001500150324     D Kpjba         E DS
001600150324     d trads           ds
001700150324     d  tra_nrr              397    400b 0
001800150324     d*---
001900150324     d dtaiso          s               d   datfmt(*iso)
002000150324     d dtaeur          s               d   datfmt(*eur)
002100150324     D tracdfds      E DS
002200150325     d  svCkau         s                   like(c1Ckau)
002300150325     d  svCdes         s                   like(c1Cdes)
002400150325     d posizionamento...
002500150325     d                 s              1A
002600020603      *
002700020531     d NotUsed         S             16A
002800020531     d ReturnSize      S              9B 0
002900020531     d SizeList        S              9B 0
003000020531     d WrkSort         S              1
003100020531     d*---
003200150324     d Rec1            S              5  0                                      *n° rel sf
003300150324     d Lastrec1        S              5  0                                      *n° rel sf x stampa
003400020531     d*---
003500150325      *  Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
003600150325      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
003700150325     d MaxKey          C                   3
003800150325     d*---
003900020531     d Ascendente      C                   1
004000020531     d Discendente     C                   2
004100020531     d Carattere       C                   6
004200020531     d Put             C                   1
004300020531     d EndPut          C                   2
004400020531     d Get             C                   3
004500020603     d Numerico        C                   8
004600020531     d*---
004700020531     d SflRcd          DS
004800150324     d  vkau
004900150324     d  vdes
005000150324     d  vbpt
005100150325     d  hdin
005200150325     d  vdin
005300150324     d  vdfi
005400150324     d  vnaz
005500150324     d  hnrr
005600020531     d  Selected                      1A
005700020531
005800020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
005900020531     d QLGKL                         16    DIM(MaxKey)
006000020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
006100020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
006200020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
006300020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
006400020531
006500020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
006600020531      /COPY QSYSINC/QRPGLESRC,QUSEC
006700971203     C*------------------------------------------------------------------------*
006800971203     C* MAIN LINE
006900971203     C*------------------------------------------------------------------------*
007000971203     C     *ENTRY        PLIST
007100150324     C                   PARM                    kpjba
007200150324      *
007300150324     c                   seton                                        LR
007400150325     c                   seton                                        50
007500150325      * Indicare una DATA limite da cui partire
007600150325     C                   WRITE     SD24D1                                       *
007700150325     c                   eval      dadata = 01012014
007800150325     c     data_err      tag
007900150325     C                   exfmt     SD24DAT                                      *
008000150325     c*   F3 = Fine
008100150325     c                   if        *inkc
008200150325     c                   return
008300150325     c                   end
008400150325     c*
008500150325      * Data da controllare
008600150325     c                   clear                   hdaData
008700150325$017 C     dadata        IFgt      *zeros
008800150325     c     *eur          test(d)                 dadata                 77
008900150325     c                   if        *in77
009000150325     c     *dmy          test(d)                 DaData                 77
009100150325     c  n77*dmy          move      DaData        dtaeur
009200150325     c  n77              move      dtaeur        DaDAta
009300150325     C                   end
009400150325     C                   end
009500150325     c                   if        DaData <> *zeros and *in77
009600150325     c                   goto      data_err
009700150325     C                   ENDIF
009800150325      *
009900150325      * gira la data in AMG
010000150325$017 C     DaData        IFgt      *zeros
010100150325     c                   move      DaData        dtaeur
010200150325     c                   move      dtaeur        dtaiso
010300150325     c                   move      dtaiso        hDaData
010400150325     C                   end
010500150325      **
010600150325     c*-----------------------------------------------------------------
010700150324      * Carica il SFL
010800150324     c                   exsr      carsfl
010900150325     C                   EXSR      F11_Ordina
011000150324     C                   WRITE     SD24D1                                       *
011100150324     c*
011200150324     c     VIDEO         tag
011300150324     C                   WRITE     SD24Z1                                       *
011400150324     c*
011500000000     C*---------- EMISSIONE SFL :                          *
011600020709     C                   EXFMT     SD24DCT                                      *
011700150324     c*   F3 = Fine
011800150324     c                   if        *inkc
011900150324     c                   return
012000150324     c                   end
012100150324     c*
012200150324     C                   move      *off          $prima_emis
012300020604      *
012400020603     C* F11 = Cambia ordinamento
012500020603if  1C                   IF        *INKK
012600150325     C                   EXSR      F11_Ordina
012700020603e   1C                   ENDIF
012800020604      *
012900150325     C                   EXSR      CTRsfl
013000150324      *
013100150325     c  nKF              goto      VIDEO
013200150325      *
013300150325     C                   EXSR      Aggiorna
013400150325      *
013500150324     c                   return
013600150324      *--------------------------------------------------------------------------------------------*
013700150324      * carica il SFL x i records da aggionrare
013800150324      *--------------------------------------------------------------------------------------------*
013900150324     C     carSFL        BEGSR
014000150324      *
014100150324     C*---------- PULIZIA SLFCT :                          *
014200150324     C                   SETON                                            70    *
014300150324     C                   WRITE     SD24DCT                                      *
014400150324     C                   SETOFF                                       7071      *
014500150324     C*----------
014600150325     C                   move      *blanks       C1CDes
014700150325     C                   move      *blanks       C1CKau
014800150325     C                   move      *blanks       svCDes
014900150325     C                   move      *blanks       svCKau
015000150324     C                   move      *on           $prima_emis       1
015100150324     C                   setoff                                       15
015200150324     C                   Z-ADD     0             REC1                           *n° record sfile
015300150324      *
015400150324     C                   DO        *hival                                       *
015500150324     C                   READ      aitra03l                               91
015600150324     C                   if        %eof(aitra03l)                               *
015700150324     c                             or   REC1 =9999
015800150324     c                   leave
015900150324     c                   end
016000150324      *
016100150324     c                   movel     tracdf        tracdfds
016200150324      *
016300150324     c                   if        tradfi = 0
016400150324     c                               or
016500150324     c                             TRAITAEST <> ' '
016600150324     c                               or
016700150324     c                             TRAsco = 'S'
016800150324     c                               or
016900150325     c                             TRAann <>' '
017000150325     c                               or
017100150325     c                             TRAdfi < HdaDATA
017200150324     c                   iter
017300150324     c                   end
017400150324     C*---
017500150324     C                   EXSR      CampiSFL
017600150324     C*---
017700150324     C                   ADD       1             REC1                 71        *
017800150324     C                   WRITE     SD24DSF                                      *
017900150324      *
018000150324     C                   enddo                                                  *
018100150324      *
018200150325     C                   EVAL      RcdNbr  = 1
018300150324     C                   Z-ADD     REC1          LastREC1                       *n° rel sf x stampa
018400150324      *
018500150324     c                   endsr
018600020603      *--------------------------------------------------------------------------------------------*
018700020603      * Valorizzo campo tipo flag tipo p.o
018800020603      *--------------------------------------------------------------------------------------------*
018900150324     C     CampiSFL      BEGSR
019000020603      *
019100150324     c                   move      trakau        vkau
019200150324     c                   move      trabpt        vbpt
019300150324     c                   movel     traaut        vdes
019400150324     c                   clear                   vnaz
019500150325     c                   move      tradin        Hdin
019600150325     c                   move      tradin        dtaiso
019700150324     c                   move      dtaiso        dtaeur
019800150324     c                   move      dtaeur        vdin
019900150324     c                   move      tradfi        dtaiso
020000150324     c                   move      dtaiso        dtaeur
020100150324     c                   move      dtaeur        vdfi
020200150324     c                   z-add     tra_nrr       hnrr
020300020603      *
020400020603     C                   ENDSR
020500020531      *--------------------------------------------------------------------------------------------*
020600150325      * F11_Ordina - TASTO FUNZIONE F11 --> Ragione sociale/codice
020700020531      *--------------------------------------------------------------------------------------------*
020800150325     C     F11_Ordina    BEGSR
020900150324      *
021000020710     C                   setoff                                       72
021100150324     C                   CLEAR                   C1Ckau
021200150324     C                   CLEAR                   C1Cdes
021300150325     C                   CLEAR                   svCkau
021400150325     C                   CLEAR                   svCdes
021500150325      *  dal primo record
021600020613     C                   EVAL      rcdnbr  = 1
021700020531      *
021800020531     C                   CLEAR                   QLGSCB
021900020531     C                   CLEAR                   QLGSCB00
022000150325      *
022100150324     ** DESCRIZIONE AUTISTA è il primo
022200020603     C                   IF        *in50=*off
022300150325      * quante Chiave
022400150325     C                   EVAL      QLGNBRK    = 3
022500150325      *
022600150324     ** 1 campo chiave. codice AUT num., ascending.
022700150324     C                   EVAL      QLGSP      = 1
022800150324     C                   EVAL      QLGSS      = %SIZE(VKAU)
022900150324     C                   EVAL      QLGDT      = Numerico
023000020531     C                   EVAL      QLGSO      = Ascendente
023100150324     C                   EVAL      QLGKL(1)   = QLGSKL
023200150325      *
023300150325     ***2 campo chiave. codice BPT num., ascending.
023400150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
023500150325     C                   EVAL      QLGSS      = %SIZE(VBPT)
023600150325     C                   EVAL      QLGDT      = Numerico
023700150325     C                   EVAL      QLGSO      = Ascendente
023800150325     C                   EVAL      QLGKL(2)   = QLGSKL
023900150325      *
024000150325     ***3 campo chiave. Data Accreditamente Ascending.
024100150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
024200150325     C                                            + %SIZE(VBPT)
024300150325     C                   EVAL      QLGSS      = %SIZE(HDIN)
024400150325     C                   EVAL      QLGDT      = Numerico
024500150325     C                   EVAL      QLGSO      = Ascendente
024600150325     C                   EVAL      QLGKL(3)   = QLGSKL
024700020603     C                   seton                                        50
024800150324      *
024900020603     C                   ELSE
025000020603      *
025100150325      * quante Chiave
025200150325     C                   EVAL      QLGNBRK    = 2
025300150325      *
025400150325     ** 1 campo chiave. Ragione sociale , char, ascending.
025500150324     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU)
025600150324     C                   EVAL      QLGSS      = %SIZE(VDES)
025700020603     C                   EVAL      QLGDT      = Carattere
025800020603     C                   EVAL      QLGSO      = Ascendente
025900020603     C                   EVAL      QLGKL(1)   = QLGSKL
026000150325      *
026100150325     ***2 campo chiave. Data Accreditamente Ascending.
026200150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
026300150325     C                                            + %SIZE(VBPT)
026400150325     C                   EVAL      QLGSS      = %SIZE(HDIN)
026500150325     C                   EVAL      QLGDT      = Numerico
026600150325     C                   EVAL      QLGSO      = Ascendente
026700150325     C                   EVAL      QLGKL(2)   = QLGSKL
026800020603     C                   setoff                                       50
026900150325      *
027000020603     C                   ENDIF
027100020531
027200020531      * Load other sort parameters.
027300020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
027400020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
027500020531     C                   EVAL      QLGRT     = 8
027600020531     C                   EVAL      QLGOKL    = 80
027700020531     C                   EVAL      QLGLKE    = 16
027800020531     C                   EVAL      QLGLSS    = 290
027900020531
028000020531      * Initialize Sort I/O API fields.
028100020531     C                   EVAL      QLGRL00  = QLGRL
028200020531     C                   EVAL      QLGRC00  = 1
028300020531     C                   CLEAR                   QUSEI
028400020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028500020531
028600020531      * First step - Initialize the sort routine.
028700020531     C                   CALL      'QLGSORT'
028800020531     C                   PARM                    QLGSCB
028900020531     C                   PARM                    NotUsed
029000020531     C                   PARM                    NotUsed
029100020531     C                   PARM                    SizeList
029200020531     C                   PARM                    ReturnSize
029300020531     C                   PARM                    QUSEC
029400020531
029500020531      * Next step - Write records to I/O routine.
029600020531     C                   EVAL      QLGRT00 = Put
029700020531
029800150324     C                   DO        LastREC1      rec1
029900020531
030000150324     C     rec1          CHAIN     SD24DSF
030100020531
030200020531     ** Solo le righe con Selected = 'Y' sono riordinate,
030300020531     ** quindi per fare un ordinamento di tutte le righe
030400020531     ** metto 'Y' sempre.
030500020531     C                   EVAL      Selected = 'Y'
030600020531
030700020531     C                   CLEAR                   QUSEI
030800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030900020531     C                   CALL      'QLGSRTIO'
031000020531     C                   PARM                    QLGSCB00
031100020531     C                   PARM                    SflRcd
031200020531     C                   PARM                    NotUsed
031300020531     C                   PARM                    SizeList
031400020531     C                   PARM                    NotUsed
031500020531     C                   PARM                    QUSEC
031600020531
031700020531     C                   ENDDO
031800020531
031900020531      * Next step - Signal end of input, clear subfile for reload.
032000020531     C                   EVAL      QLGRT00 = EndPut
032100020531     C                   CLEAR                   QUSEI
032200020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032300020531
032400020531     C                   CALL      'QLGSRTIO'
032500020531     C                   PARM                    QLGSCB00
032600020531     C                   PARM                    SflRcd
032700020531     C                   PARM                    NotUsed
032800020531     C                   PARM                    SizeList
032900020531     C                   PARM                    NotUsed
033000020531     C                   PARM                    QUSEC
033100020531
033200020531     C                   SETON                                            70    *
033300020531     C                   WRITE     SD24DCT                                      *
033400020603     C                   SETOFF                                       70        *
033500020531
033600020531      * Final step - Write the records back to the subfile.
033700020531     C                   EVAL      QLGRT00 = Get
033800020531
033900150324     C                   DO        LastREC1      rec1
034000020531     C                   CLEAR                   QUSEI
034100020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
034200020531     C                   CALL      'QLGSRTIO'
034300020531     C                   PARM                    QLGSCB00
034400020531     C                   PARM                    NotUsed
034500020531     C                   PARM                    SflRcd
034600020531     C                   PARM                    QLGRL00
034700020531     C                   PARM                    NotUsed
034800020531     C                   PARM                    QUSEC
034900020531      *
035000020531     C                   WRITE     SD24DSF
035100020531     C                   ENDDO
035200020531
035300020531     C                   EVAL      *IN70 = *off
035400020604      *
035500020531     C                   ENDSR
035600020606      *--------------------------------------------------------------------------------------------*
035700150325      * CTRsfl Subroutine per il riposizionamento
035800020606      *--------------------------------------------------------------------------------------------*
035900150325     C     CTRsfl        BEGSR
036000020613      *
036100150325      *  posizionamento x Codice
036200150325     c                   if         svCKAU  <> c1cKAU
036300150325     c                   eval      svCKAU = c1cKAU
036400150325     c                   exsr      posiz_KAU
036500150325      *
036600150325      *  posizionamento x Descrizione
036700150325     c                   elseif     svCDES  <> c1cDES
036800150325     c                   eval      svCdes = c1cDES
036900150325     c                   exsr      posiz_DES
037000150325      *
037100150325     c                   end
037200020613      *
037300020606     C                   ENDSR
037400150325      *--------------------------------------------------------------------------------------------*
037500150325      * Posizionamento x Codice
037600150325      *--------------------------------------------------------------------------------------------*
037700150325     C     Posiz_KAU     BEGSR
037800150325      *
037900150325     C                   EVAL      RcdNbr  = 1
038000150325      *
038100150325     C                   DO        LastREC1      rec1
038200150325     C     rec1          CHAIN     SD24DSF
038300150325      *
038400150325     c                   if        svcKAU < VKAU
038500150325     C                   EVAL      RcdNbr  = rec1
038600150325     c                   leave
038700150325     c                   end
038800150325      *
038900150325      *
039000150325     C                   ENDDO
039100150325      *
039200150325     C                   ENDSR
039300150325      *--------------------------------------------------------------------------------------------*
039400150325      * Posizionamento x Ragione Sociale
039500150325      *--------------------------------------------------------------------------------------------*
039600150325     C     Posiz_DES     BEGSR
039700150325      *
039800150325     C                   EVAL      RcdNbr  = 1
039900150325      *
040000150325     C                   DO        LastREC1      rec1
040100150325     C     rec1          CHAIN     SD24DSF
040200150325      *
040300150325     c                   if        svcDES < VDES
040400150325     C                   EVAL      RcdNbr  = rec1
040500150325     c                   leave
040600150325     c                   end
040700150325      *
040800150325      *
040900150325     C                   ENDDO
041000150325      *
041100150325     C                   ENDSR
041200150325      *--------------------------------------------------------------------------------------------*
041300150325      * Aggiornamento  I/E
041400150325      *--------------------------------------------------------------------------------------------*
041500150325     C     Aggiorna      BEGSR
041600150325      *
041700150325     C                   DO        LastREC1      rec1
041800150325     C     rec1          CHAIN     SD24DSF
041900150325      *
042000150325      * Nazione I/E
042100150325     c                   if        VNAZ <> *blank
042200150325     C     HNRR          CHAIN     aitra00F
042300150325      *
042400150325     c                   movel     tracdf        tracdfds
042500150325     c                   eval      TRAITAEST = Vnaz
042600150325     c                   eval      tracdf = tracdfds
042700150325      *
042800150325     c                   update    aitraFIS
042900150325     c                   end
043000150325      *
043100150325     C                   ENDDO
043200150325      *
043300150325     C                   ENDSR
043400150325      *--------------------------------------------------------------------------------------------*
