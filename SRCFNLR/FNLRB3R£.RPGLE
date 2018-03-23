000100030703     H DECEDIT('0,') DATEDIT(*DMY/)
000200940726      * FNLRB5R *----------------------------------------------------*
000300030702      *
000400030702      *          VISUALIZZAZIONE NOTE SUPERMERCATI/BANCALI
000500030702      *
000600940726      *--------------------------------------------------------------*
000700940726      *  Indicatori utilizzati:
000800030121      *  01   ON   Interrogazione da Sede
000900030122      *  10   ON   Mancato reso bancali
001000940726      *  20   ON   SFLDSP
001100940726      *  21   ON   SFLDSPCTL
001200940726      *  21   OFF  SFLCLR
001300021212      *  30   Ciclo di lettura su file
001400021212      *  31   Generico
001500940726      *--------------------------------------------------------------*
001600021212     fFiar501l  if   e           k disk    usropn
001700040531     fFiar531c  if   e           k disk    usropn
001800021212     f                                     rename (fiar5000:fiar500S)
001900021212     fTabel00f  if   e           k disk
002000021212     fAzOrg01l  if   e           k disk
002100021212     fFnLrb3d   cf   e             workstn sfile(LRB3S01:NRR1)
002200030122
002300021212     d kpjba         e ds
002400030702     d Dar5Scr       e ds
002500030702     d Dar5Ban       e ds
002600021213     d FnLrb3Ds      e ds
002700030702     d Ds1P          e ds
002800030122     d Tibs02Ds      e ds
002900030122     d dTba          e ds
003000030702
003100030703     d wcodtba         s              2
003200030703     d wdestba         s             10
003300030703     d wcodtc          s              2
003400030703     d wdestc          s             25
003500030703     d wdata           s              8  0
003600030703     d wora            s              4  0
003700030702
003800021212      * Ds per la routine di controllo data  (8 CIFRE)
003900021212     d WLBDAT          DS                  INZ
004000021212     d  G02DAT                 1      8  0
004100021212     d  G02INV                 9     16  0
004200021212     d  G02ERR                17     17
004300021212     d  G02TGI                18     22  0
004400030702
004500030703     d KEYpar          ds             8    inz
004600030703     d  WTPpar                 1      1    inz('S')
004700030703     d  WCDpar                 2      3    inz
004800030703     d  Wprogr                 4      8    inz('V0001')
004900030703
005000021212      * Constanti
005100030703     d weditora        c                   '  :  '
005200030703     d weditpo         c                   '0   '
005300021212      *--------------------------------------------------------------*     *****
005400021212     c     *ENTRY        PLIST
005500030122     c                   PARM                    Kpjba
005600030121     c                   PARM                    Fnlrb3Ds
005700030702
005800030122     c                   Movel     Rb3Rsu        VttRsu
005900021212     c                   move      1             kkut
006000030702
006100021211      * Apertura file
006200030121     c                   IF        Rb3Fil = *Zeros
006300021211      * Sede
006400040531     c                   open      Fiar531c
006500021212     c                   eval      *in01 = *on
006600021212     c                   ELSE
006700021211      * Filiale
006800021212     c                   open      Fiar501l
006900021212     c                   eval      *in01 = *off
007000020920     c                   ENDIF
007100030122
007200030122     c                   Eval      *In10 = *Off
007300021212      *
007400030702      *         P U L I Z I A   S U B F I L E
007500021212      *
007600030702     c                   Z-ADD     0             NRR1              4 0
007700021212     c                   eval      *in21 = *off
007800021212     c                   WRITE     LRB3C01
007900030702     c                   eval      *in20 = *on
008000030702     c                   eval      *in21 = *on
008100030702
008200030702      * Imposto i campi del control
008300021212     c                   Exsr      RieCtl
008400030702      *
008500030702      *         C A R I C O    S U B F I L E
008600030702      *
008700030702      * B A N C A L I  non ritirati
008800030122     c                   move      'BAN'         $Trd
008900030702     c                   Clear                   dAr5Ban
009000030122      * Sede
009100040531     c   01Karb5         Chain     Fiar531C                           30
009200030122      * Filiale
009300030122     c  n01Karb5         Chain     Fiar501l                           30
009400030122
009500030702      * Imposto i campi solo se ci sono dei resi
009600030702If  1c                   If        Not *In30 and Ar5Atb = *Blanks
009700030702     c                   Movel     Ar5Uni        dAr5Ban
009800030702If  2c                   If        §Ar5Rb1 > *Zeros or §Ar5Rb2 > *Zeros
009900030702      * Riempio i campi del Sfl x record "BAN"
010000030702     C                   EXSR      RieSflBan
010100030702    2c                   EndIf
010200030127    1c                   EndIf
010300030702
010400030702      * S U P E R M E R C A T I
010500030702     c                   move      'SCR'         $Trd
010600021212      *
010700021212      * Leggo il file bolle dall'ultimo record valido
010800021212     C                   IF        *in01 = *on
010900021212      * Sede
011000040531     C     KARB5         SETGT     FIAR531C
011100040531     C     KARB5         READPE    FIAR531C                               30
011200021212      * Filiale
011300021212     C                   ELSE
011400021212     C     KARB5         SETGT     FIAR501L
011500021212     C     KARB5         READPE    FIAR501L                               30
011600021212     C                   ENDIF
011700021212      *
011800930525     C     *IN30         DOWEQ     '0'
011900030702     C                   Clear                   Dar5Scr
012000021212      * Escludo i record annullati
012100021212     C                   IF        Ar5Atb = *blanks
012200021212     C                   eval      Dar5Scr = Ar5Uni
012300030702      * Riempio i campi del Sfl x record "SCR"
012400030702     C                   EXSR      RieSflScr
012500930525     C                   END
012600030702
012700021212      * Letture Sede/Filiale
012800021212     C                   IF        *in01 = *on
012900040531     C     KARB5         READPE    FIAR531C                               30
013000021212     C                   ELSE
013100021212     C     KARB5         READPE    FIAR501L                               30
013200021212     C                   ENDIF
013300021212     C                   ENDDO
013400021212      *
013500030702      *         E M E T T O    S U B F I L E
013600021212      *
013700021218     C     NRR1          IFNE      0
013800021212     C     *INKL         DOWEQ     *off
013900021212     C                   Z-ADD     1             NRR1
014000940725     C                   Z-ADD     1             REC1
014100021212     C                   WRITE     LRB3T01                                      INTESTAZIONE VIDEO
014200021212     C                   WRITE     LRB3Z01                                      TASTI FUNZIONALI
014300021212     C                   EXFMT     LRB3C01
014400021212     C                   ENDDO
014500021212     C                   ENDIF
014600030122      * Subfile vuoto emetto una videata diversa
014700030122     c                   If        Nrr1 = *Zeros
014800030122     c                   Dow       Not *Inkl
014900030122     C                   WRITE     LRB3T01                                      INTESTAZIONE VIDEO
015000030122     C                   WRITE     LRB3Z01                                      TASTI FUNZIONALI
015100030122     C                   EXFMT     LRB3D01
015200030122     c                   EndDo
015300030122     c                   EndIf
015400021212      *
015500030122     C                   SETON                                        LR        PREMUTO F12=FINE
015600021212      **************************************************************************
015700021212      *    Riempio i campi del control
015800021212      **************************************************************************
015900021212     C     RieCtl        BegSr
016000021212      *
016100021212     C                   z-add     Rb3Lnp        V1CLnp
016200021212     C                   z-add     Rb3Nrs        V1CNrs
016300021212     C                   z-add     Rb3Nsp        V1CNsp
016400021212      *
016500021216     C                   z-add     Rb3Dsp        V1CDsp
016600021212     C                   z-add     Rb3Rmn        V1CRmn
016700021212     C                   z-add     Rb3Lna        V1CLna
016800021212     C     Rb3Lna        chain     AzOrg01l                           31
016900021212     C  n31              movel(p)  OrgDes        V1CLnd
017000021212     C   31              movel     *blanks       V1CLnd
017100021212     C                   movel     Rb3Rsd        V1CRsd
017200021218     C                   z-add     Rb3Pkf        V1CPkf
017300021212     C                   z-add     Rb3Ncl        V1CNcl
017400021212     C                   z-add     Rb3Vlb        V1CVlb
017500030703      * Bancali - x rcd BAN o x rcd BNB
017600030122     C                   z-add     Rb3Nba        V1CNba
017700030122     C                   z-add     Rb3Nb2        V1CNb2
017800030702If  1c                   If        Rb3Tba <> *Zeros
017900030703     c                   Movel     Rb3Tba        wcodtba
018000030702     c                   ExSr      Sr_DecTba
018100030703     c                   Eval      V1dTba = wdestba
018200030702    1c                   EndIf
018300030702If  1c                   If        Rb3Tb2 <> *Zeros
018400030703     c                   Movel     Rb3Tb2        wcodtba
018500030702     c                   ExSr      Sr_DecTba
018600030703     c                   Eval      V1dTb2 = wdestba
018700030702    1c                   EndIf
018800030703      * Particolarità varie
018900030703     c                   Clear                   V1dGva
019000030703     c                   If        Rb3Gva <> *Blanks
019100030703     c                   Eval      V1cGva = Rb3Gva
019200030703     c                   Eval      %subst(V1cGva:2:1) = ' '
019300030703     c                   movel     '7V'          kcod
019400030703     c                   Movel     V1cGva        wcdpar
019500030703     c                   movel(p)  keypar        kkey
019600030703     c     ktab          chain     tabel00f
019700030703     c                   If        %Found(Tabel00f) and Tblflg = *Blanks
019800030703     c                   Movel     tbluni        V1dGva
019900030703     c                   EndIf
020000030703     c                   EndIf
020100030703      * Colli
020200030703     c                   Eval      V1cCsf = Rb3Csf
020300030703     c                   Eval      V1cCor = Rb3Cor
020400021212      *
020500021212     C                   EndSr
020600030702      **************************************************************************
020700030702      *    Riempio i campi del subfile record BAN
020800030702      **************************************************************************
020900030702     c     RieSflBan     BegSr
021000030702
021100030703      * giro la data - §Ar5Dav
021200030703     c                   Z-Add     §Ar5Dav       G02Inv
021300030703     c                   Z-Add     *Zeros        G02Dat
021400030703     c                   Movel     '3'           G02Err
021500030703     c                   Call      'XSRDA8'
021600030703     c                   Parm                    WlbDat
021700030703     c                   Z-Add     G02Dat        wdata
021800030703      * imposto l'ora da 6 a 4 - §Ar5Orv
021900030703     c                   Movel     §Ar5Orv       wora
022000030703
022100030702      * P r i m a   r i g a
022200030703     c                   Eval      V1sRcd =
022300030703     c                                     'Mancato ritiro bancali ins. da: ' +
022400030703     c                                      §Ar5Prub
022500030703     c                   Eval      V1sRcd =
022600030703     c                                     %trim(V1sRcd) +
022700030703     c                                     ' del p.o. ' +
022800030703     c                                     %trim(%editw(§Ar5Poub:weditpo)) +
022900030703     c                                     ' il ' + %trim(%editc(wdata:'Y')) +
023000030703     c                                     ' alle ' + %editw(wora:weditora)
023100030703     c                   Add       1             Nrr1
023200030703     c                   Write     LRB3S01
023300030703     c                   Clear                   V1sRcd
023400030702
023500030702      * S e c o n d a   r i g a
023600030703     c                   Eval      V1sRcd =
023700030703     c                                     'Bancali non ritirati: '
023800030703If  2c                   If        §Ar5Rb1 > *Zeros
023900030703     c                   Eval      V1sRcd =
024000030703     c                                     %trim(V1sRcd) + ' ' +
024100030703     c                                     %trim(%editc(§Ar5Rb1:'4')) + ' ' +
024200030703     c                                     V1dTba + ' / '
024300030703    2c                   EndIf
024400030703If  2c                   If        §Ar5Rb2 > *Zeros
024500030703     c                   Eval      V1sRcd =
024600030703     c                                     %trim(V1sRcd) + ' ' +
024700030703     c                                     %trim(%editc(§Ar5Rb2:'4')) + ' ' +
024800030703     c                                     V1dTb2
024900030703    2c                   EndIf
025000030703     c                   Add       1             Nrr1
025100030703     c                   Write     LRB3S01
025200030703     c                   Clear                   V1sRcd
025300030702
025400030702      * T e r z a   r i g a
025500030703     c                   Eval      V1sRcd =
025600030703     c                                     'Ritiro negato da: ' + §Ar5Nomb
025700030703     c                   Add       1             Nrr1
025800030703     c                   Write     LRB3S01
025900030703     c                   Clear                   V1sRcd
026000030702
026100030702      * Q u a r t a   r i g a
026200030703     c                   Eval      V1sRcd =
026300030703     c                                     'Motivo: ' + §Ar5Motb
026400030703     c                   Add       1             Nrr1
026500030703     c                   Write     LRB3S01
026600030703     c                   Clear                   V1sRcd
026700030702
026800030702     C                   EndSr
026900021212      **************************************************************************
027000030702      *    Riempio i campi del subfile record SCR
027100021212      **************************************************************************
027200030702     c     RieSflScr     BegSr
027300030703
027400030703     c                   If        Nrr1 > *Zeros
027500030703     c                   Eval      V1sRcd = *all'_'
027600030703     c                   Add       1             Nrr1
027700030703     c                   Write     LRB3S01
027800030703     c                   Clear                   V1sRcd
027900030703     c                   EndIf
028000030702
028100030703      * giro la data - Ar5Dac
028200030703     c                   Z-Add     Ar5Dac        G02Inv
028300030703     c                   Z-Add     *Zeros        G02Dat
028400030703     c                   Movel     '3'           G02Err
028500030703     c                   Call      'XSRDA8'
028600030703     c                   Parm                    WlbDat
028700030703     c                   Z-Add     G02Dat        wdata
028800030703      * imposto l'ora da 6 a 4 - Ar5Orc
028900030703     c                   Movel     Ar5Orc        wora
029000030703
029100030702      * P r i m a   r i g a
029200030703     c                   Eval      V1sRcd =
029300030703     c                                     'Variazione effettuata da: ' +
029400030703     c                                     §Ar5Pru
029500030703     c                   Eval      V1sRcd =
029600030703     c                                     %trim(V1sRcd) +
029700030703     c                                     ' del p.o. ' +
029800030703     c                                     %trim(%editw(§Ar5Pou:weditpo)) +
029900030703     c                                     ' il ' + %trim(%editc(wdata:'Y')) +
030000030703     c                                     ' alle ' + %editw(wora:weditora)
030100030703     c                   Add       1             Nrr1
030200030703     c                   Write     LRB3S01
030300030703     c                   Clear                   V1sRcd
030400030702
030500030702      * S e c o n d a   r i g a
030600030702If  1c                   If        §Ar5Dcr <> *Zeros or §Ar5Hcr <> *Zeros or
030700030702     c                             §Ar5Gc1 <> *Blanks or §Ar5Gc2 <> *Blanks
030800030703If  2c                   If        §Ar5Dcr <> *Zeros
030900030703      * giro la data - §Ar5Dcr
031000030703     c                   Z-Add     §Ar5Dcr       G02Inv
031100030703     c                   Z-Add     *Zeros        G02Dat
031200030703     c                   Movel     '3'           G02Err
031300030703     c                   Call      'XSRDA8'
031400030703     c                   Parm                    WlbDat
031500030703     c                   Z-Add     G02Dat        wdata
031600030703
031700030703     c                   Eval      V1sRcd =
031800030703     c                                     'Consegna richiesta '
031900030703s   3c                   SELECT
032000030703     c                   When      §ar5Tcr = *blanks
032100030703     c                   Eval      V1sRcd =
032200030703     c                                     %trim(V1sRcd) + ' il: '
032300030703     c                   When      §ar5Tcr = 'D'
032400030703     c                   Eval      V1sRcd =
032500030703     c                                     %trim(V1sRcd) + ' dopo il: '
032600021212     c                   When      §ar5Tcr = 'P'
032700030703     c                   Eval      V1sRcd =
032800030703     c                                     %trim(V1sRcd) + ' prima del: '
032900030703    3c                   EndSl
033000030703     c                   Eval      V1sRcd =
033100030703     c                                     %trim(V1sRcd) + ' ' +
033200030703     c                                     %trim(%editc(wdata:'Y'))
033300030703    2c                   EndIf
033400030703If  2c                   If        §Ar5Hcr <> *Zeros
033500030703If  3c                   If        §Ar5Dcr = *Zeros
033600030703     c                   Eval      V1sRcd =
033700030703     c                                     'Consegna richiesta '
033800030703s   4c                   SELECT
033900030703     c                   When      §ar5Tcr = *blanks
034000030703     c                   Eval      V1sRcd =
034100030703     c                                     %trim(V1sRcd) + ' alle: '
034200030703     c                   When      §ar5Tcr = 'D'
034300030703     c                   Eval      V1sRcd =
034400030703     c                                     %trim(V1sRcd) + ' dopo le: '
034500030703     c                   When      §ar5Tcr = 'P'
034600030703     c                   Eval      V1sRcd =
034700030703     c                                     %trim(V1sRcd) + ' prima delle: '
034800030703    4c                   EndSl
034900030703    3c                   EndIf
035000030703     c                   Eval      V1sRcd =
035100030703     c                                     %trim(V1sRcd) + ' ' +
035200030703     c                                     %editw(§Ar5Hcr:weditora)
035300030703    2c                   EndIf
035400030703If  2c                   If        §Ar5Gc1 <> *Blanks or §Ar5Gc2 <> *Blanks
035500030703If  3c                   If        %subst(V1sRcd:1:1) <> *Blanks
035600030703     c                   Eval      V1sRcd =
035700030703     c                                     %trim(V1sRcd) + ' ' +
035800030703     c                                     'Giorni di Chiusura: '
035900030703   x3c                   Else
036000030703     c                   Eval      V1sRcd =
036100030703     c                                     %trim(V1sRcd) +
036200030703     c                                     'Giorni di Chiusura: '
036300030703    3c                   EndIf
036400030703     c                   Eval      V1sRcd =
036500030703     c                                     %trim(V1sRcd) + ' ' +
036600030703     c                                     §Ar5Gc1 + ' ' + §Ar5Gc2
036700030703    2c                   EndIf
036800030703     c                   Add       1             Nrr1
036900030703     c                   Write     LRB3S01
037000030703     c                   Clear                   V1sRcd
037100030702    1c                   EndIf
037200030702
037300030702      * T e r z a   r i g a
037400030702      * Consegne particolari
037500030702If  1c                   If        §Ar5Tc1 <> *blanks or §Ar5Tc2 <> *Blanks
037600030703     c                   Eval      V1sRcd =
037700030703     c                                     'Consegne particolari:'
037800030702If  2c                   If        §Ar5Tc1 <> *blanks
037900030703     c                   Movel     §Ar5Tc1       wcodtc
038000030703     c                   ExSr      Sr_DecTc
038100030703     c                   Eval      V1sRcd =
038200030703     c                                     %trim(V1sRcd) + ' ' + §Ar5Tc1 +
038300030703     c                                     '-' + wdestc
038400030702    2c                   EndIf
038500030702      * Seconda consegna particolare
038600030702If  2c                   If        §Ar5Tc2 <> *blanks
038700030703     c                   Movel     §Ar5Tc2       wcodtc
038800030703     c                   ExSr      Sr_DecTc
038900030703     c                   Eval      V1sRcd =
039000030703     c                                     %trim(V1sRcd) + ' ' + §Ar5Tc2 +
039100030703     c                                     '-' + wdestc
039200030702    2c                   EndIf
039300030703     c                   Add       1             Nrr1
039400030703     c                   Write     LRB3S01
039500030703     c                   Clear                   V1sRcd
039600030702    1c                   EndIf
039700030702
039800030702      * Q u a r t a   r i g a
039900030703     c                   Eval      V1sRcd =
040000030703     c                                     'Riferimento contatto: ' + §Ar5Nom
040100030703     c                   Eval      V1sRcd =
040200030703     c                                     %trim(V1sRcd) +
040300030703     c                                     '  Tel:' + §Ar5Tel
040400030703     c                   Add       1             Nrr1
040500030703     c                   Write     LRB3S01
040600030703     c                   Clear                   V1sRcd
040700030702
040800030702      * Q u i n t a   r i g a
040900030703     c                   Eval      V1sRcd =
041000030703     c                                     'Motivo: ' + §Ar5Mot
041100030703     c                   Add       1             Nrr1
041200030703     c                   Write     LRB3S01
041300030703     c                   Clear                   V1sRcd
041400030702
041500021212     c                   EndSr
041600030702      **************************************************************************
041700030702      *    Decodifico il tipo bancale
041800030702      **************************************************************************
041900030702     c     Sr_DecTba     BegSr
042000030702
042100030702     c                   Clear                   Tibs02Ds
042200030702     c                   Clear                   dTba
042300030702     c                   Movel     'C'           T02Mod
042400030702     c                   Movel     Knsif         T02Sif
042500030702     c                   Movel     'TBA'         T02Cod
042600030703     c                   Movel(p)  wcodtba       T02Ke1
042700030702     c                   Call      'TIBS02R'
042800030702     c                   Parm                    Kpjba
042900030702     c                   Parm                    Tibs02Ds
043000030702     c                   If        T02Err = *Blanks
043100030702     c                   Movel     T02Uni        dTba
043200030702     c                   EndIf
043300030703     c                   Eval      wdestba = §TbaDesc
043400030702
043500030702     c                   EndSr
043600030702      **************************************************************************
043700030702      *    Decodifico la consegna particolare
043800030702      **************************************************************************
043900030702     c     Sr_DecTc      BegSr
044000030702
044100030702     c                   Clear                   Ds1p
044200030702     c                   movel     '1P'          kcod
044300030703     c                   movel(p)  wcodtc        kkey
044400030702     c     ktab          chain     tabel00f
044500030702     c                   If        %Found(Tabel00f) and Tblflg = *Blanks
044600030702     c                   Movel     tbluni        ds1P
044700030702     c                   EndIf
044800030703     c                   Movel     §1PDes        wdestc
044900030702
045000030702     c                   EndSr
045100021212      **************************************************************************
045200021212      *    R O U T I N E     I N I Z I A L E
045300021212      **************************************************************************
045400940726     C     *INZSR        BEGSR
045500021212      *
045600021212     C     Karb5         KLIST
045700021212     C                   KFLD                    Rb3Aas                         ..ANNO DI 4 CIFRE
045800021212     C                   KFLD                    Rb3Lnp
045900021212     C                   KFLD                    Rb3Nrs
046000021212     C                   KFLD                    Rb3Nsp
046100030702     C                   KFLD                    $Trd              3
046200021212      *
046300021212     C     KTab          KLIST
046400030122     C                   KFLD                    KKut              1 0
046500021212     C                   KFLD                    KCod              2
046600021212     C                   KFLD                    KKey              8
046700021212      *
046800940726     C                   ENDSR
