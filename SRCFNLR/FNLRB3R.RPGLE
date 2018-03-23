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
002700051128     d fnlv80ds      e ds
002800030702     d Ds1P          e ds
002900030122     d Tibs02Ds      e ds
003000030122     d dTba          e ds
003100030702
003200030703     d wcodtba         s              2
003300030703     d wdestba         s             10
003400030703     d wcodtc          s              2
003500030703     d wdestc          s             25
003600030703     d wdata           s              8  0
003700030703     d wora            s              4  0
003800030702
003900021212      * Ds per la routine di controllo data  (8 CIFRE)
004000021212     d WLBDAT          DS                  INZ
004100021212     d  G02DAT                 1      8  0
004200021212     d  G02INV                 9     16  0
004300021212     d  G02ERR                17     17
004400021212     d  G02TGI                18     22  0
004500030702
004600030703     d KEYpar          ds             8    inz
004700030703     d  WTPpar                 1      1    inz('S')
004800030703     d  WCDpar                 2      3    inz
004900030703     d  Wprogr                 4      8    inz('V0001')
005000030703
005100021212      * Constanti
005200030703     d weditora        c                   '  :  '
005300030703     d weditpo         c                   '0   '
005400021212      *--------------------------------------------------------------*     *****
005500021212     c     *ENTRY        PLIST
005600030122     c                   PARM                    Kpjba
005700030121     c                   PARM                    Fnlrb3Ds
005800030702
005900030122     c                   Movel     Rb3Rsu        VttRsu
006000021212     c                   move      1             kkut
006100030702
006200021211      * Apertura file
006300030121     c                   IF        Rb3Fil = *Zeros
006400021211      * Sede
006500040531     c                   open      Fiar531c
006600021212     c                   eval      *in01 = *on
006700021212     c                   ELSE
006800021211      * Filiale
006900021212     c                   open      Fiar501l
007000021212     c                   eval      *in01 = *off
007100020920     c                   ENDIF
007200030122
007300030122     c                   Eval      *In10 = *Off
007400021212      *
007500030702      *         P U L I Z I A   S U B F I L E
007600021212      *
007700030702     c                   Z-ADD     0             NRR1              4 0
007800021212     c                   eval      *in21 = *off
007900021212     c                   WRITE     LRB3C01
008000030702     c                   eval      *in20 = *on
008100030702     c                   eval      *in21 = *on
008200030702
008300030702      * Imposto i campi del control
008400021212     c                   Exsr      RieCtl
008500030702      *
008600030702      *         C A R I C O    S U B F I L E
008700030702      *
008800030702      * B A N C A L I  non ritirati
008900030122     c                   move      'BAN'         $Trd
009000030702     c                   Clear                   dAr5Ban
009100030122      * Sede
009200040531     c   01Karb5         Chain     Fiar531C                           30
009300030122      * Filiale
009400030122     c  n01Karb5         Chain     Fiar501l                           30
009500030122
009600030702      * Imposto i campi solo se ci sono dei resi
009700030702If  1c                   If        Not *In30 and Ar5Atb = *Blanks
009800030702     c                   Movel     Ar5Uni        dAr5Ban
009900030702If  2c                   If        §Ar5Rb1 > *Zeros or §Ar5Rb2 > *Zeros
010000030702      * Riempio i campi del Sfl x record "BAN"
010100030702     C                   EXSR      RieSflBan
010200030702    2c                   EndIf
010300030127    1c                   EndIf
010400030702
010500030702      * S U P E R M E R C A T I
010600030702     c                   move      'SCR'         $Trd
010700021212      *
010800021212      * Leggo il file bolle dall'ultimo record valido
010900021212     C                   IF        *in01 = *on
011000021212      * Sede
011100040531     C     KARB5         SETGT     FIAR531C
011200040531     C     KARB5         READPE    FIAR531C                               30
011300021212      * Filiale
011400021212     C                   ELSE
011500021212     C     KARB5         SETGT     FIAR501L
011600021212     C     KARB5         READPE    FIAR501L                               30
011700021212     C                   ENDIF
011800021212      *
011900930525     C     *IN30         DOWEQ     '0'
012000030702     C                   Clear                   Dar5Scr
012100021212      * Escludo i record annullati
012200021212     C                   IF        Ar5Atb = *blanks
012300021212     C                   eval      Dar5Scr = Ar5Uni
012400030702      * Riempio i campi del Sfl x record "SCR"
012500030702     C                   EXSR      RieSflScr
012600930525     C                   END
012700030702
012800021212      * Letture Sede/Filiale
012900021212     C                   IF        *in01 = *on
013000040531     C     KARB5         READPE    FIAR531C                               30
013100021212     C                   ELSE
013200021212     C     KARB5         READPE    FIAR501L                               30
013300021212     C                   ENDIF
013400021212     C                   ENDDO
013500021212      *
013600030702      *         E M E T T O    S U B F I L E
013700021212      *
013800021218     C     NRR1          IFNE      0
013900021212     C     *INKL         DOWEQ     *off
014000021212     C                   Z-ADD     1             NRR1
014100940725     C                   Z-ADD     1             REC1
014200021212     C                   WRITE     LRB3T01                                      INTESTAZIONE VIDEO
014300021212     C                   WRITE     LRB3Z01                                      TASTI FUNZIONALI
014400021212     C                   EXFMT     LRB3C01
014500021212     C                   ENDDO
014600021212     C                   ENDIF
014700030122      * Subfile vuoto emetto una videata diversa
014800030122     c                   If        Nrr1 = *Zeros
014900030122     c                   Dow       Not *Inkl
015000030122     C                   WRITE     LRB3T01                                      INTESTAZIONE VIDEO
015100030122     C                   WRITE     LRB3Z01                                      TASTI FUNZIONALI
015200030122     C                   EXFMT     LRB3D01
015300030122     c                   EndDo
015400030122     c                   EndIf
015500021212      *
015600030122     C                   SETON                                        LR        PREMUTO F12=FINE
015700021212      **************************************************************************
015800021212      *    Riempio i campi del control
015900021212      **************************************************************************
016000021212     C     RieCtl        BegSr
016100021212      *
016200021212     C                   z-add     Rb3Lnp        V1CLnp
016300021212     C                   z-add     Rb3Nrs        V1CNrs
016400021212     C                   z-add     Rb3Nsp        V1CNsp
016500021212      *
016600021216     C                   z-add     Rb3Dsp        V1CDsp
016700021212     C                   z-add     Rb3Rmn        V1CRmn
016800021212     C                   z-add     Rb3Lna        V1CLna
016900021212     C     Rb3Lna        chain     AzOrg01l                           31
017000021212     C  n31              movel(p)  OrgDes        V1CLnd
017100021212     C   31              movel     *blanks       V1CLnd
017200021212     C                   movel     Rb3Rsd        V1CRsd
017300021218     C                   z-add     Rb3Pkf        V1CPkf
017400021212     C                   z-add     Rb3Ncl        V1CNcl
017500021212     C                   z-add     Rb3Vlb        V1CVlb
017600030703      * Bancali - x rcd BAN o x rcd BNB
017700030122     C                   z-add     Rb3Nba        V1CNba
017800030122     C                   z-add     Rb3Nb2        V1CNb2
017900030702If  1c                   If        Rb3Tba <> *Zeros
018000030703     c                   Movel     Rb3Tba        wcodtba
018100030702     c                   ExSr      Sr_DecTba
018200030703     c                   Eval      V1dTba = wdestba
018300030702    1c                   EndIf
018400030702If  1c                   If        Rb3Tb2 <> *Zeros
018500030703     c                   Movel     Rb3Tb2        wcodtba
018600030702     c                   ExSr      Sr_DecTba
018700030703     c                   Eval      V1dTb2 = wdestba
018800030702    1c                   EndIf
018900030703      * Particolarità varie
019000030703     c                   Clear                   V1dGva
019100030703     c                   If        Rb3Gva <> *Blanks
019200030703     c                   Eval      V1cGva = Rb3Gva
019300030703     c                   Eval      %subst(V1cGva:2:1) = ' '
019400030703     c                   movel     '7V'          kcod
019500030703     c                   Movel     V1cGva        wcdpar
019600030703     c                   movel(p)  keypar        kkey
019700030703     c     ktab          chain     tabel00f
019800030703     c                   If        %Found(Tabel00f) and Tblflg = *Blanks
019900030703     c                   Movel     tbluni        V1dGva
020000030703     c                   EndIf
020100030703     c                   EndIf
020200030703      * Colli
020300030703     c                   Eval      V1cCsf = Rb3Csf
020400030703     c                   Eval      V1cCor = Rb3Cor
020500021212      *
020600021212     C                   EndSr
020700030702      **************************************************************************
020800030702      *    Riempio i campi del subfile record BAN
020900030702      **************************************************************************
021000030702     c     RieSflBan     BegSr
021100030702
021200030703      * giro la data - §Ar5Dav
021300030703     c                   Z-Add     §Ar5Dav       G02Inv
021400030703     c                   Z-Add     *Zeros        G02Dat
021500030703     c                   Movel     '3'           G02Err
021600030703     c                   Call      'XSRDA8'
021700030703     c                   Parm                    WlbDat
021800030703     c                   Z-Add     G02Dat        wdata
021900030703      * imposto l'ora da 6 a 4 - §Ar5Orv
022000030703     c                   Movel     §Ar5Orv       wora
022100030703
022200030702      * P r i m a   r i g a
022300030703     c                   Eval      V1sRcd =
022400030703     c                                     'Mancato ritiro bancali ins. da: ' +
022500030703     c                                      §Ar5Prub
022600030703     c                   Eval      V1sRcd =
022700030703     c                                     %trim(V1sRcd) +
022800030703     c                                     ' del p.o. ' +
022900030703     c                                     %trim(%editw(§Ar5Poub:weditpo)) +
023000030703     c                                     ' il ' + %trim(%editc(wdata:'Y')) +
023100030703     c                                     ' alle ' + %editw(wora:weditora)
023200030703     c                   Add       1             Nrr1
023300030703     c                   Write     LRB3S01
023400030703     c                   Clear                   V1sRcd
023500030702
023600030702      * S e c o n d a   r i g a
023700030703     c                   Eval      V1sRcd =
023800030703     c                                     'Bancali non ritirati: '
023900030703If  2c                   If        §Ar5Rb1 > *Zeros
024000030703     c                   Eval      V1sRcd =
024100030703     c                                     %trim(V1sRcd) + ' ' +
024200030703     c                                     %trim(%editc(§Ar5Rb1:'4')) + ' ' +
024300030703     c                                     V1dTba + ' / '
024400030703    2c                   EndIf
024500030703If  2c                   If        §Ar5Rb2 > *Zeros
024600030703     c                   Eval      V1sRcd =
024700030703     c                                     %trim(V1sRcd) + ' ' +
024800030703     c                                     %trim(%editc(§Ar5Rb2:'4')) + ' ' +
024900030703     c                                     V1dTb2
025000030703    2c                   EndIf
025100030703     c                   Add       1             Nrr1
025200030703     c                   Write     LRB3S01
025300030703     c                   Clear                   V1sRcd
025400030702
025500030702      * T e r z a   r i g a
025600030703     c                   Eval      V1sRcd =
025700030703     c                                     'Ritiro negato da: ' + §Ar5Nomb
025800030703     c                   Add       1             Nrr1
025900030703     c                   Write     LRB3S01
026000030703     c                   Clear                   V1sRcd
026100030702
026200030702      * Q u a r t a   r i g a
026300030703     c                   Eval      V1sRcd =
026400030703     c                                     'Motivo: ' + §Ar5Motb
026500030703     c                   Add       1             Nrr1
026600030703     c                   Write     LRB3S01
026700030703     c                   Clear                   V1sRcd
026800030702
026900030702     C                   EndSr
027000021212      **************************************************************************
027100030702      *    Riempio i campi del subfile record SCR
027200021212      **************************************************************************
027300030702     c     RieSflScr     BegSr
027400030703
027500030703     c                   If        Nrr1 > *Zeros
027600030703     c                   Eval      V1sRcd = *all'_'
027700030703     c                   Add       1             Nrr1
027800030703     c                   Write     LRB3S01
027900030703     c                   Clear                   V1sRcd
028000030703     c                   EndIf
028100030702
028200030703      * giro la data - Ar5Dac
028300030703     c                   Z-Add     Ar5Dac        G02Inv
028400030703     c                   Z-Add     *Zeros        G02Dat
028500030703     c                   Movel     '3'           G02Err
028600030703     c                   Call      'XSRDA8'
028700030703     c                   Parm                    WlbDat
028800030703     c                   Z-Add     G02Dat        wdata
028900030703      * imposto l'ora da 6 a 4 - Ar5Orc
029000030703     c                   Movel     Ar5Orc        wora
029100030703
029200030702      * P r i m a   r i g a
029300030703     c                   Eval      V1sRcd =
029400030703     c                                     'Variazione effettuata da: ' +
029500030703     c                                     §Ar5Pru
029600030703     c                   Eval      V1sRcd =
029700030703     c                                     %trim(V1sRcd) +
029800030703     c                                     ' del p.o. ' +
029900030703     c                                     %trim(%editw(§Ar5Pou:weditpo)) +
030000030703     c                                     ' il ' + %trim(%editc(wdata:'Y')) +
030100030703     c                                     ' alle ' + %editw(wora:weditora)
030200030703     c                   Add       1             Nrr1
030300030703     c                   Write     LRB3S01
030400030703     c                   Clear                   V1sRcd
030500030702
030600030702      * S e c o n d a   r i g a
030700030702If  1c                   If        §Ar5Dcr <> *Zeros or §Ar5Hcr <> *Zeros or
030800030702     c                             §Ar5Gc1 <> *Blanks or §Ar5Gc2 <> *Blanks
030900051128     c
031000051128If  2c                   If        §Ar5Dcr <> *Zeros  or §ar5hcr>0
031100030703
031200030703     c                   Eval      V1sRcd =
031300030703     c                                     'Consegna richiesta '
031400051128     c                   clear                   fnlv80ds
031500051128     c                   movel     'L'           ilv80tla
031600051128     c                   movel     rb3dsp        w0040             4 0
031700051128     c                   move      w0040         wmese             2 0
031800051128     c                   movel     w0040         wgiorno           2 0
031900051128     c                   eval      ilv80aas=rb3aas
032000051128     c                   eval      ilv80lnp=rb3lnp
032100051128     c                   eval      ilv80nrs=rb3nrs
032200051128     c                   eval      ilv80nsp=rb3nsp
032300051128     c                   movel     wmese         ilv80mgs
032400051128     c                   move      wgiorno       ilv80mgs
032500051128     c                   eval      ilv80tc1=§ar5tc1
032600051128     c                   eval      ilv80tc2=§ar5tc2
032700051128     c                   eval      ilv80dcr=§ar5dcr
032800051128     c                   eval      ilv80hcr=§ar5hcr
032900051128     c                   eval      ilv80tcr=§ar5tcr
033000051128     c                   call      'FNLV80R'
033100051128     c                   parm                    kpjba
033200051128     c                   parm                    fnlv80ds
033300051128     c
033400051128     c                   eval      wdata=olv80dcr8
033500051128     c                   Eval      V1sRcd =
033600051128     c                              %trim(V1sRcd) +' ' + %trim(olv80tcrd)
033700051128     c                             +': '
033800051128     c*
033900051128     c                   if        wdata>0
034000030703     c                   Eval      V1sRcd =
034100030703     c                                     %trim(V1sRcd) + ' ' +
034200030703     c                                     %trim(%editc(wdata:'Y'))
034300051128     c                   endif
034400051128     c                   if        §ar5hcr>0
034500030703     c                   Eval      V1sRcd =
034600030703     c                                     %trim(V1sRcd) + ' ' +
034700030703     c                                     %editw(§Ar5Hcr:weditora)
034800030703    2c                   EndIf
034900051128    2c                   EndIf
035000051128     c
035100030703If  2c                   If        §Ar5Gc1 <> *Blanks or §Ar5Gc2 <> *Blanks
035200030703If  3c                   If        %subst(V1sRcd:1:1) <> *Blanks
035300030703     c                   Eval      V1sRcd =
035400030703     c                                     %trim(V1sRcd) + ' ' +
035500030703     c                                     'Giorni di Chiusura: '
035600030703   x3c                   Else
035700030703     c                   Eval      V1sRcd =
035800030703     c                                     %trim(V1sRcd) +
035900030703     c                                     'Giorni di Chiusura: '
036000030703    3c                   EndIf
036100030703     c                   Eval      V1sRcd =
036200030703     c                                     %trim(V1sRcd) + ' ' +
036300030703     c                                     §Ar5Gc1 + ' ' + §Ar5Gc2
036400030703    2c                   EndIf
036500030703     c                   Add       1             Nrr1
036600030703     c                   Write     LRB3S01
036700030703     c                   Clear                   V1sRcd
036800030702    1c                   EndIf
036900030702
037000030702      * T e r z a   r i g a
037100030702      * Consegne particolari
037200030702If  1c                   If        §Ar5Tc1 <> *blanks or §Ar5Tc2 <> *Blanks
037300030703     c                   Eval      V1sRcd =
037400030703     c                                     'Consegne particolari:'
037500030702If  2c                   If        §Ar5Tc1 <> *blanks
037600030703     c                   Movel     §Ar5Tc1       wcodtc
037700030703     c                   ExSr      Sr_DecTc
037800030703     c                   Eval      V1sRcd =
037900030703     c                                     %trim(V1sRcd) + ' ' + §Ar5Tc1 +
038000030703     c                                     '-' + wdestc
038100030702    2c                   EndIf
038200030702      * Seconda consegna particolare
038300030702If  2c                   If        §Ar5Tc2 <> *blanks
038400030703     c                   Movel     §Ar5Tc2       wcodtc
038500030703     c                   ExSr      Sr_DecTc
038600030703     c                   Eval      V1sRcd =
038700030703     c                                     %trim(V1sRcd) + ' ' + §Ar5Tc2 +
038800030703     c                                     '-' + wdestc
038900030702    2c                   EndIf
039000030703     c                   Add       1             Nrr1
039100030703     c                   Write     LRB3S01
039200030703     c                   Clear                   V1sRcd
039300030702    1c                   EndIf
039400030702
039500030702      * Q u a r t a   r i g a
039600030703     c                   Eval      V1sRcd =
039700030703     c                                     'Riferimento contatto: ' + §Ar5Nom
039800030703     c                   Eval      V1sRcd =
039900030703     c                                     %trim(V1sRcd) +
040000030703     c                                     '  Tel:' + §Ar5Tel
040100030703     c                   Add       1             Nrr1
040200030703     c                   Write     LRB3S01
040300030703     c                   Clear                   V1sRcd
040400030702
040500030702      * Q u i n t a   r i g a
040600030703     c                   Eval      V1sRcd =
040700030703     c                                     'Motivo: ' + §Ar5Mot
040800030703     c                   Add       1             Nrr1
040900030703     c                   Write     LRB3S01
041000030703     c                   Clear                   V1sRcd
041100030702
041200021212     c                   EndSr
041300030702      **************************************************************************
041400030702      *    Decodifico il tipo bancale
041500030702      **************************************************************************
041600030702     c     Sr_DecTba     BegSr
041700030702
041800030702     c                   Clear                   Tibs02Ds
041900030702     c                   Clear                   dTba
042000030702     c                   Movel     'C'           T02Mod
042100030702     c                   Movel     Knsif         T02Sif
042200030702     c                   Movel     'TBA'         T02Cod
042300030703     c                   Movel(p)  wcodtba       T02Ke1
042400030702     c                   Call      'TIBS02R'
042500030702     c                   Parm                    Kpjba
042600030702     c                   Parm                    Tibs02Ds
042700030702     c                   If        T02Err = *Blanks
042800030702     c                   Movel     T02Uni        dTba
042900030702     c                   EndIf
043000030703     c                   Eval      wdestba = §TbaDesc
043100030702
043200030702     c                   EndSr
043300030702      **************************************************************************
043400030702      *    Decodifico la consegna particolare
043500030702      **************************************************************************
043600030702     c     Sr_DecTc      BegSr
043700030702
043800030702     c                   Clear                   Ds1p
043900030702     c                   movel     '1P'          kcod
044000030703     c                   movel(p)  wcodtc        kkey
044100030702     c     ktab          chain     tabel00f
044200030702     c                   If        %Found(Tabel00f) and Tblflg = *Blanks
044300030702     c                   Movel     tbluni        ds1P
044400030702     c                   EndIf
044500030703     c                   Movel     §1PDes        wdestc
044600030702
044700030702     c                   EndSr
044800021212      **************************************************************************
044900021212      *    R O U T I N E     I N I Z I A L E
045000021212      **************************************************************************
045100940726     C     *INZSR        BEGSR
045200021212      *
045300021212     C     Karb5         KLIST
045400021212     C                   KFLD                    Rb3Aas                         ..ANNO DI 4 CIFRE
045500021212     C                   KFLD                    Rb3Lnp
045600021212     C                   KFLD                    Rb3Nrs
045700021212     C                   KFLD                    Rb3Nsp
045800030702     C                   KFLD                    $Trd              3
045900021212      *
046000021212     C     KTab          KLIST
046100030122     C                   KFLD                    KKut              1 0
046200021212     C                   KFLD                    KCod              2
046300021212     C                   KFLD                    KKey              8
046400021212      *
046500940726     C                   ENDSR
