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
002100150325     d  svCkau         s                   like(c1Ckau)
002200150325     d  svCdes         s                   like(c1Cdes)
002300150325     d posizionamento...
002400150325     d                 s              1A
002500020603      *
002600020531     d NotUsed         S             16A
002700020531     d ReturnSize      S              9B 0
002800020531     d SizeList        S              9B 0
002900020531     d WrkSort         S              1
003000020531     d*---
003100150324     d Rec1            S              5  0                                      *n° rel sf
003200150324     d Lastrec1        S              5  0                                      *n° rel sf x stampa
003300020531     d*---
003400150325      *  Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
003500150325      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
003600150325     d MaxKey          C                   3
003700150325     d*---
003800020531     d Ascendente      C                   1
003900020531     d Discendente     C                   2
004000020531     d Carattere       C                   6
004100020531     d Put             C                   1
004200020531     d EndPut          C                   2
004300020531     d Get             C                   3
004400020603     d Numerico        C                   8
004500020531     d*---
004600020531     d SflRcd          DS
004700150324     d  vkau
004800150324     d  vdes
004900150324     d  vbpt
005000150325     d  hdin
005100150325     d  vdin
005200150324     d  vdfi
005300150324     d  vnaz
005400150324     d  hnrr
005500020531     d  Selected                      1A
005600020531
005700020531      /COPY QSYSINC/QRPGLESRC,QLGSORT
005800020531     d QLGKL                         16    DIM(MaxKey)
005900020531     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
006000020531     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
006100020531     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
006200020531     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
006300020531
006400020531      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
006500020531      /COPY QSYSINC/QRPGLESRC,QUSEC
006600971203     C*------------------------------------------------------------------------*
006700971203     C* MAIN LINE
006800971203     C*------------------------------------------------------------------------*
006900971203     C     *ENTRY        PLIST
007000150324     C                   PARM                    kpjba
007100150324      *
007200150324     c                   seton                                        LR
007300150325     c                   seton                                        50
007400150325      * Indicare una DATA limite da cui partire
007500150325     C                   WRITE     SD24D1                                       *
007600150325     c                   eval      dadata = 01012014
007700150325     c     data_err      tag
007800150325     C                   exfmt     SD24DAT                                      *
007900150325     c*   F3 = Fine
008000150325     c                   if        *inkc
008100150325     c                   return
008200150325     c                   end
008300150325     c*
008400150325      * Data da controllare
008500150325     c                   clear                   hdaData
008600150325$017 C     dadata        IFgt      *zeros
008700150325     c     *eur          test(d)                 dadata                 77
008800150325     c                   if        *in77
008900150325     c     *dmy          test(d)                 DaData                 77
009000150325     c  n77*dmy          move      DaData        dtaeur
009100150325     c  n77              move      dtaeur        DaDAta
009200150325     C                   end
009300150325     C                   end
009400150325     c                   if        DaData <> *zeros and *in77
009500150325     c                   goto      data_err
009600150325     C                   ENDIF
009700150325      *
009800150325      * gira la data in AMG
009900150325$017 C     DaData        IFgt      *zeros
010000150325     c                   move      DaData        dtaeur
010100150325     c                   move      dtaeur        dtaiso
010200150325     c                   move      dtaiso        hDaData
010300150325     C                   end
010400150325      **
010500150325     c*-----------------------------------------------------------------
010600150324      * Carica il SFL
010700150324     c                   exsr      carsfl
010800150325     C                   EXSR      F11_Ordina
010900150324     C                   WRITE     SD24D1                                       *
011000150324     c*
011100150324     c     VIDEO         tag
011200150324     C                   WRITE     SD24Z1                                       *
011300150324     c*
011400000000     C*---------- EMISSIONE SFL :                          *
011500020709     C                   EXFMT     SD24DCT                                      *
011600150324     c*   F3 = Fine
011700150324     c                   if        *inkc
011800150324     c                   return
011900150324     c                   end
012000150324     c*
012100150324     C                   move      *off          $prima_emis
012200020604      *
012300020603     C* F11 = Cambia ordinamento
012400020603if  1C                   IF        *INKK
012500150325     C                   EXSR      F11_Ordina
012600020603e   1C                   ENDIF
012700020604      *
012800150325     C                   EXSR      CTRsfl
012900150324      *
013000150325     c  nKF              goto      VIDEO
013100150325      *
013200150325     C                   EXSR      Aggiorna
013300150325      *
013400150324     c                   return
013500150324      *--------------------------------------------------------------------------------------------*
013600150324      * carica il SFL x i records da aggionrare
013700150324      *--------------------------------------------------------------------------------------------*
013800150324     C     carSFL        BEGSR
013900150324      *
014000150324     C*---------- PULIZIA SLFCT :                          *
014100150324     C                   SETON                                            70    *
014200150324     C                   WRITE     SD24DCT                                      *
014300150324     C                   SETOFF                                       7071      *
014400150324     C*----------
014500150325     C                   move      *blanks       C1CDes
014600150325     C                   move      *blanks       C1CKau
014700150325     C                   move      *blanks       svCDes
014800150325     C                   move      *blanks       svCKau
014900150324     C                   move      *on           $prima_emis       1
015000150324     C                   setoff                                       15
015100150324     C                   Z-ADD     0             REC1                           *n° record sfile
015200150324      *
015300150324     C                   DO        *hival                                       *
015400150324     C                   READ      aitra03l                               91
015500150324     C                   if        %eof(aitra03l)                               *
015600150324     c                             or   REC1 =9999
015700150324     c                   leave
015800150324     c                   end
015900150324      *
016000150324     c                   if        tradfi = 0
016100150324     c                               or
016200150324     c                             TRAITAEST <> ' '
016300150324     c                               or
016400150324     c                             TRAsco = 'S'
016500150324     c                               or
016600150325     c                             TRAann <>' '
016700150325     c                               or
016800150325     c                             TRAdfi < HdaDATA
016900150324     c                   iter
017000150324     c                   end
017100150324     C*---
017200150324     C                   EXSR      CampiSFL
017300150324     C*---
017400150324     C                   ADD       1             REC1                 71        *
017500150324     C                   WRITE     SD24DSF                                      *
017600150324      *
017700150324     C                   enddo                                                  *
017800150324      *
017900150325     C                   EVAL      RcdNbr  = 1
018000150324     C                   Z-ADD     REC1          LastREC1                       *n° rel sf x stampa
018100150324      *
018200150324     c                   endsr
018300020603      *--------------------------------------------------------------------------------------------*
018400020603      * Valorizzo campo tipo flag tipo p.o
018500020603      *--------------------------------------------------------------------------------------------*
018600150324     C     CampiSFL      BEGSR
018700020603      *
018800150324     c                   move      trakau        vkau
018900150324     c                   move      trabpt        vbpt
019000150324     c                   movel     traaut        vdes
019100150324     c                   clear                   vnaz
019200150325     c                   move      tradin        Hdin
019300150325     c                   move      tradin        dtaiso
019400150324     c                   move      dtaiso        dtaeur
019500150324     c                   move      dtaeur        vdin
019600150324     c                   move      tradfi        dtaiso
019700150324     c                   move      dtaiso        dtaeur
019800150324     c                   move      dtaeur        vdfi
019900150324     c                   z-add     tra_nrr       hnrr
020000020603      *
020100020603     C                   ENDSR
020200020531      *--------------------------------------------------------------------------------------------*
020300150325      * F11_Ordina - TASTO FUNZIONE F11 --> Ragione sociale/codice
020400020531      *--------------------------------------------------------------------------------------------*
020500150325     C     F11_Ordina    BEGSR
020600150324      *
020700020710     C                   setoff                                       72
020800150324     C                   CLEAR                   C1Ckau
020900150324     C                   CLEAR                   C1Cdes
021000150325     C                   CLEAR                   svCkau
021100150325     C                   CLEAR                   svCdes
021200150325      *  dal primo record
021300020613     C                   EVAL      rcdnbr  = 1
021400020531      *
021500020531     C                   CLEAR                   QLGSCB
021600020531     C                   CLEAR                   QLGSCB00
021700150325      *
021800150324     ** DESCRIZIONE AUTISTA è il primo
021900020603     C                   IF        *in50=*off
022000150325      * quante Chiave
022100150325     C                   EVAL      QLGNBRK    = 3
022200150325      *
022300150324     ** 1 campo chiave. codice AUT num., ascending.
022400150324     C                   EVAL      QLGSP      = 1
022500150324     C                   EVAL      QLGSS      = %SIZE(VKAU)
022600150324     C                   EVAL      QLGDT      = Numerico
022700020531     C                   EVAL      QLGSO      = Ascendente
022800150324     C                   EVAL      QLGKL(1)   = QLGSKL
022900150325      *
023000150325     ***2 campo chiave. codice BPT num., ascending.
023100150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
023200150325     C                   EVAL      QLGSS      = %SIZE(VBPT)
023300150325     C                   EVAL      QLGDT      = Numerico
023400150325     C                   EVAL      QLGSO      = Ascendente
023500150325     C                   EVAL      QLGKL(2)   = QLGSKL
023600150325      *
023700150325     ***3 campo chiave. Data Accreditamente Ascending.
023800150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
023900150325     C                                            + %SIZE(VBPT)
024000150325     C                   EVAL      QLGSS      = %SIZE(HDIN)
024100150325     C                   EVAL      QLGDT      = Numerico
024200150325     C                   EVAL      QLGSO      = Ascendente
024300150325     C                   EVAL      QLGKL(3)   = QLGSKL
024400020603     C                   seton                                        50
024500150324      *
024600020603     C                   ELSE
024700020603      *
024800150325      * quante Chiave
024900150325     C                   EVAL      QLGNBRK    = 2
025000150325      *
025100150325     ** 1 campo chiave. Ragione sociale , char, ascending.
025200150324     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU)
025300150324     C                   EVAL      QLGSS      = %SIZE(VDES)
025400020603     C                   EVAL      QLGDT      = Carattere
025500020603     C                   EVAL      QLGSO      = Ascendente
025600020603     C                   EVAL      QLGKL(1)   = QLGSKL
025700150325      *
025800150325     ***2 campo chiave. Data Accreditamente Ascending.
025900150325     C                   EVAL      QLGSP      = 1 + %SIZE(VKAU) + %SIZE(VDES)
026000150325     C                                            + %SIZE(VBPT)
026100150325     C                   EVAL      QLGSS      = %SIZE(HDIN)
026200150325     C                   EVAL      QLGDT      = Numerico
026300150325     C                   EVAL      QLGSO      = Ascendente
026400150325     C                   EVAL      QLGKL(2)   = QLGSKL
026500020603     C                   setoff                                       50
026600150325      *
026700020603     C                   ENDIF
026800020531
026900020531      * Load other sort parameters.
027000020531     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
027100020531     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
027200020531     C                   EVAL      QLGRT     = 8
027300020531     C                   EVAL      QLGOKL    = 80
027400020531     C                   EVAL      QLGLKE    = 16
027500020531     C                   EVAL      QLGLSS    = 290
027600020531
027700020531      * Initialize Sort I/O API fields.
027800020531     C                   EVAL      QLGRL00  = QLGRL
027900020531     C                   EVAL      QLGRC00  = 1
028000020531     C                   CLEAR                   QUSEI
028100020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
028200020531
028300020531      * First step - Initialize the sort routine.
028400020531     C                   CALL      'QLGSORT'
028500020531     C                   PARM                    QLGSCB
028600020531     C                   PARM                    NotUsed
028700020531     C                   PARM                    NotUsed
028800020531     C                   PARM                    SizeList
028900020531     C                   PARM                    ReturnSize
029000020531     C                   PARM                    QUSEC
029100020531
029200020531      * Next step - Write records to I/O routine.
029300020531     C                   EVAL      QLGRT00 = Put
029400020531
029500150324     C                   DO        LastREC1      rec1
029600020531
029700150324     C     rec1          CHAIN     SD24DSF
029800020531
029900020531     ** Solo le righe con Selected = 'Y' sono riordinate,
030000020531     ** quindi per fare un ordinamento di tutte le righe
030100020531     ** metto 'Y' sempre.
030200020531     C                   EVAL      Selected = 'Y'
030300020531
030400020531     C                   CLEAR                   QUSEI
030500020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030600020531     C                   CALL      'QLGSRTIO'
030700020531     C                   PARM                    QLGSCB00
030800020531     C                   PARM                    SflRcd
030900020531     C                   PARM                    NotUsed
031000020531     C                   PARM                    SizeList
031100020531     C                   PARM                    NotUsed
031200020531     C                   PARM                    QUSEC
031300020531
031400020531     C                   ENDDO
031500020531
031600020531      * Next step - Signal end of input, clear subfile for reload.
031700020531     C                   EVAL      QLGRT00 = EndPut
031800020531     C                   CLEAR                   QUSEI
031900020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
032000020531
032100020531     C                   CALL      'QLGSRTIO'
032200020531     C                   PARM                    QLGSCB00
032300020531     C                   PARM                    SflRcd
032400020531     C                   PARM                    NotUsed
032500020531     C                   PARM                    SizeList
032600020531     C                   PARM                    NotUsed
032700020531     C                   PARM                    QUSEC
032800020531
032900020531     C                   SETON                                            70    *
033000020531     C                   WRITE     SD24DCT                                      *
033100020603     C                   SETOFF                                       70        *
033200020531
033300020531      * Final step - Write the records back to the subfile.
033400020531     C                   EVAL      QLGRT00 = Get
033500020531
033600150324     C                   DO        LastREC1      rec1
033700020531     C                   CLEAR                   QUSEI
033800020531     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
033900020531     C                   CALL      'QLGSRTIO'
034000020531     C                   PARM                    QLGSCB00
034100020531     C                   PARM                    NotUsed
034200020531     C                   PARM                    SflRcd
034300020531     C                   PARM                    QLGRL00
034400020531     C                   PARM                    NotUsed
034500020531     C                   PARM                    QUSEC
034600020531      *
034700020531     C                   WRITE     SD24DSF
034800020531     C                   ENDDO
034900020531
035000020531     C                   EVAL      *IN70 = *off
035100020604      *
035200020531     C                   ENDSR
035300020606      *--------------------------------------------------------------------------------------------*
035400150325      * CTRsfl Subroutine per il riposizionamento
035500020606      *--------------------------------------------------------------------------------------------*
035600150325     C     CTRsfl        BEGSR
035700020613      *
035800150325      *  posizionamento x Codice
035900150325     c                   if         svCKAU  <> c1cKAU
036000150325     c                   eval      svCKAU = c1cKAU
036100150325     c                   exsr      posiz_KAU
036200150325      *
036300150325      *  posizionamento x Descrizione
036400150325     c                   elseif     svCDES  <> c1cDES
036500150325     c                   eval      svCdes = c1cDES
036600150325     c                   exsr      posiz_DES
036700150325      *
036800150325     c                   end
036900020613      *
037000020606     C                   ENDSR
037100150325      *--------------------------------------------------------------------------------------------*
037200150325      * Posizionamento x Codice
037300150325      *--------------------------------------------------------------------------------------------*
037400150325     C     Posiz_KAU     BEGSR
037500150325      *
037600150325     C                   EVAL      RcdNbr  = 1
037700150325      *
037800150325     C                   DO        LastREC1      rec1
037900150325     C     rec1          CHAIN     SD24DSF
038000150325      *
038100150325     c                   if        svcKAU < VKAU
038200150325     C                   EVAL      RcdNbr  = rec1
038300150325     c                   leave
038400150325     c                   end
038500150325      *
038600150325      *
038700150325     C                   ENDDO
038800150325      *
038900150325     C                   ENDSR
039000150325      *--------------------------------------------------------------------------------------------*
039100150325      * Posizionamento x Ragione Sociale
039200150325      *--------------------------------------------------------------------------------------------*
039300150325     C     Posiz_DES     BEGSR
039400150325      *
039500150325     C                   EVAL      RcdNbr  = 1
039600150325      *
039700150325     C                   DO        LastREC1      rec1
039800150325     C     rec1          CHAIN     SD24DSF
039900150325      *
040000150325     c                   if        svcDES < VDES
040100150325     C                   EVAL      RcdNbr  = rec1
040200150325     c                   leave
040300150325     c                   end
040400150325      *
040500150325      *
040600150325     C                   ENDDO
040700150325      *
040800150325     C                   ENDSR
040900150325      *--------------------------------------------------------------------------------------------*
041000150325      * Aggiornamento  I/E
041100150325      *--------------------------------------------------------------------------------------------*
041200150325     C     Aggiorna      BEGSR
041300150325      *
041400150325     C                   DO        LastREC1      rec1
041500150325     C     rec1          CHAIN     SD24DSF
041600150325      *
041700150325      * Nazione I/E
041800150325     c                   if        VNAZ <> *blank
041900150325     C     HNRR          CHAIN     aitra00F
042000150325      *
042100150325     c                   eval      TRAITAEST = Vnaz
042200150325      *
042300150325     c                   update    aitraFIS
042400150325     c                   end
042500150325      *
042600150325     C                   ENDDO
042700150325      *
042800150325     C                   ENDSR
042900150325      *--------------------------------------------------------------------------------------------*
