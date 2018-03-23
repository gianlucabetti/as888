000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - STATISTICA CONSUMI  - FILTRO -                 *
000300000000     H*             --------------------------------                 *
000400000000     H*--------------------------------------------------------------*
000500011204     FTRMZ16D   CF   E             WORKSTN
000600000000     FTABEL00F  IF   E           K DISK
000700011204     FAZORG01L  IF   e           K DISK
000800011204     Fmzame02L  IF   e           K DISK
000900150102     Fmzamh02L  IF   e           K DISK
001000011205     d param           ds
001100011205     d anno                           4  0
001200011205     d mese                           2  0
001300011205     d vscef                          1
001400011205     d vscec                          1
001500011205     d vscea                          1
001600011205     d vscet                          1
001700011205     d vfil                           3  0
001800141124     d vcat                          20
001900011205     d vaus                           2
002000011205     d vtga                           2
002100011205     d vtgn                           9
002200141124     d vvend                          1
002300160810     d vkmdal                         6  0
002400160810     d vkmal                          6  0
002500160810     d vcaut                          2
002600160902     d vfun                           5
002700160810      *
002800011207     d dtaiso          s               d
002900141124     D cat             S              2    dim(10) inz
003000000000     D KPJBA         E DS
003100000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003200160810     D tibs02ds      E DS
003300160810     D dcau          E DS
003400141124      *
003500141124     iga16d001
003600141124     i              vcat1                       cat(1)
003700141124     i              vcat2                       cat(2)
003800141124     i              vcat3                       cat(3)
003900141124     i              vcat4                       cat(4)
004000141124     i              vcat5                       cat(5)
004100141124     i              vcat6                       cat(6)
004200141124     i              vcat7                       cat(7)
004300141124     i              vcat8                       cat(8)
004400141124     i              vcat9                       cat(9)
004500141124     i              vcat10                      cat(10)
004600000000     C*---------------------------------------------------------------*
004700011204     C                   IF        NOT *INLR
004800011204     C                   EXSR      AZZER
004900011204     C                   do        *hival
005000011204     c                   EXSR      CLR00
005100011204     c                   if        *in99
005200011204     c                   setoff                                       99
005300011204     c                   write     ga16d001
005400011204     c                   seton                                        99
005500011204     c                   end
005600011204     C                   EXFMT     GA16D001
005700011204     C   Kc              leave
005800011204     C* RICERCA
005900011204     C   Kd              EXSR      ric01
006000011204     c* controlli
006100011204     C                   EXSR      ctr01
006200011204     c   99              iter
006300011204     C*CONTROLLI SUPERATI
006400011204     c                   if        *inkf
006500011205     c*
006600141124     c                   movea     cat           vcat
006700011211     C                   MOVEL     'MZ17'        KCOAZ
006800011205     C                   MOVEL(p)  param         kpjbu
006900011204     C                   CALL      'BCH09'
007000011204     C                   PARM                    KPJBA
007100011204     C     KNMUS         IFEQ      *ALL'1'
007200011212     C                   CALL      'TRMZ17C'
007300011204     C                   PARM                    KPJBA
007400011204     C                   ELSE
007500011204     C                   CALL      'BCH10'
007600011204     C                   PARM                    KPJBA
007700011204     C                   END
007800011204     C                   END
007900011204     C                   ENDdo
008000011204     C                   END
008100011204     C                   SETON                                        LR
008200011204     C*----------------------------------------------------*
008300011204     C     CTR01         BEGSR
008400011204     C*----------------------------------------------------*
008500011205     C                   SETOFF                                       99
008600011204     C                   DO
008700011204     C*PERIODO
008800141205     c                   if        mese  =  0
008900011205     c                   if        anno  = 0
009000011204     c                   seton                                        4099
009100011204     c                   leave
009200011204     c                   end
009300011205     c                   if        mese  > 12
009400011204     c                   seton                                        4199
009500011204     c                   leave
009600011204     c                   end
009700011204     c                   end
009800011204     C* ORDINAMENTO
009900000000     C     VSCEF         IFEQ      *BLANKS
010000000000     C     VSCEC         ANDEQ     *BLANKS
010100011204     C     VSCEa         ANDEQ     *BLANKS
010200011204     C     VSCEt         ANDEQ     *BLANKS
010300011204     C                   SETON                                        4599
010400011204     C                   leave
010500000000     C                   END
010600011219     c                   z-add     0             conta             2 0
010700011219     c                   if        vscef <> *blanks
010800011219     c                   add       1             conta
010900011219     c                   end
011000011219     c                   if        vscec <> *blanks
011100011219     c                   add       1             conta
011200011219     c                   end
011300011219     c                   if        vscea <> *blanks
011400011219     c                   add       1             conta
011500011219     c                   end
011600011219     c                   if        vscet <> *blanks
011700011219     c                   add       1             conta
011800011219     c                   end
011900011219     c                   if        conta > 1
012000011219     C                   SETON                                        4599
012100011219     C                   leave
012200011219     c                   end
012300000000     C** FILIALE
012400011205     c                   if        vfil <> 0
012500011204     C                   EXSR      SRVFIL
012600011204     C   99              leave
012700011212     C                   ELSE
012800011212     C                   MOVEL     *BLANKS       VDFIL
012900011204     c                   end
013000141124     C*CATEGORIA da 1 a 10
013100141124     c                   do        10            i                 2 0
013200141124     c                   if        cat(i) <> *blanks
013300141124     c                   exsr      srvCATc
013400011204     C   99              leave
013500011204     c                   end
013600141124     c                   enddo
013700011204     C*CODICE AUTISTA
013800011204     c                   if        vAUS <> *blanks
013900011204     C                   EXSR      SRVAUS
014000011204     C   99              leave
014100011212     C                   ELSE
014200011212     C                   MOVEL     *BLANKS       VDAUS
014300011204     c                   end
014400011204     C*TARGA
014500011204     C     VTGA          IFNE      *BLANKS
014600011204     C     VTGN          ANDEQ     *BLANKS
014700011204     C                   SETON                                        4699
014800011204     C   99              leave
014900011204     c                   end
015000011204     C     VTGA          IFEQ      *BLANKS
015100011204     C     VTGN          ANDNE     *BLANKS
015200011204     C                   SETON                                        4699
015300011204     C   99              leave
015400011204     c                   end
015500011204     C     VTGA          IFNE      *BLANKS
015600011204     C     VTGN          ANDNE     *BLANKS
015700011204     C     KTAR          CHAIN     mzAME02l                           46
015800150102     C   46KTAR          CHAIN     mzAMh02l                           46
015900011219     C   46              SETON                                        99
016000011219     C   99              leave
016100011204     c                   end
016200160810     C*km range
016300160810     C                   IF        vkmal < vkmdal
016400160810     C                   SETON                                        4399
016500160810     C   99              leave
016600160810     c                   end
016700160810     C** marca
016800160810     c                   if        vcaut <> *blank
016900160810     c                   clear                   tibs02ds
017000160810     c                   move      'C'           t02mod
017100160810     c                   clear                   vcautd
017200160810     C                   EXSR      SRVcaut
017300160810     C   99              leave
017400160810     C                   ELSE
017500160810     C                   MOVEL     *BLANKS       Vcautd
017600160810     c                   end
017700160810      *
017800160810     c                   enddo
017900011204     C                   ENDsr
018000011204     c**********************************************************************
018100011204     C** FILIALE
018200011204     c**********************************************************************
018300011204     C     srvfil        BEGSR
018400011204     c*
018500011205     C     vfil          CHAIN     AZORG
018600011204     C                   if        not %found(azorg01l)
018700011204     C                   SETON                                        4299
018800011204     c                   else
018900011204     C                   MOVEL     ORGDES        VDFIL
019000011204     C                   END
019100011204     c*
019200011204     C                   ENDSR
019300160810     c**********************************************************************
019400160810     C** controllo marca
019500160810     c**********************************************************************
019600160810     C     srvcaut       BEGSR
019700160810     c*
019800160810     c                   if        vcaut <> *blank and t02mod <> 'R'
019900160810     c                   movel     vcaut         t02ke1
020000160810     c                   endif
020100160810     c                   movel     'CAU'         t02cod
020200160810     c                   call      'TIBS02R'
020300160810     c                   parm                    KPJBA
020400160810     c                   parm                    TIBS02DS
020500160810     c                   if        t02err =  *blanks
020600160810     c                   movel     t02uni        dcau
020700160810     c                   movel     t02ke1        vcaut
020800160810     c                   movel     §caudes       vcautd
020900160810     c                   else
021000160810     c                   seton                                        4799
021100160810     c                   endif
021200160810     C                   ENDSR
021300011204     c**********************************************************************
021400011204     C** categoria
021500011204     c**********************************************************************
021600141124     C     srvcat        BEGSR
021700011204     c*
021800011204     c                   movel     'XA'          parcod            2
021900141124     c                   movel(P)  CAT(i)        parkey
022000011204     c                   exsr      srtab
022100141124     c                   movel     parkey        cat(i)
022200011204     c                   if        *in99
022300141124     c                   z-add     49            ii                2 0
022400141124     c                   add       i             ii
022500141124     C                   eval      *in(ii) = *on
022600011204     C                   END
022700011204     c*
022800011204     C                   ENDSR
022900141124     c**********************************************************************
023000141124     C** categoria solo controllo
023100141124     c**********************************************************************
023200141124     C     srvcatc       BEGSR
023300141124     c*
023400141124     c                   movel     'XA'          cod
023500141124     c                   movel(P)  CAT(i)        key
023600141124     C     KTAB          CHAIN     TABEL                              99
023700141124     c                   if        *in99
023800141124     c                   z-add     49            ii                2 0
023900141124     c                   add       i             ii
024000141124     C                   eval      *in(ii) = *on
024100141124     C                   END
024200141124     c*
024300141124     C                   ENDSR
024400011204     c**********************************************************************
024500011204     C** autista
024600011204     c**********************************************************************
024700011204     C     srvaus        BEGSR
024800011204     c                   movel     'XN'          parcod
024900011204     c                   movel(P)  Vaus          parkey
025000011204     c                   exsr      srtab
025100011204     c                   movel     parkey        vaus
025200011204     c                   if        *in99
025300011204     C                   MOVEL     *blanks       VDAUs
025400011204     C                   SETON                                        44
025500011204     C                   else
025600011204     C                   MOVEL     TBLUNI        VDAUs
025700011204     C                   END
025800011204     c*
025900011204     C                   ENDSR
026000011204     C**********************************************************************
026100011204     C** Controlli e decodifiche campi con tabel00f
026200011204     C**********************************************************************
026300011204     C     srtab         BEGSR
026400011204     c                   if        fld <> *blanks
026500011204     c                   movel     parkey        parkeys           8
026600011204     C                   CALL      'TRUL19R'
026700011204     c                   parm                    parcod            2
026800011204     c                   parm      '1'           parord            1
026900011204     c                   parm      *blanks       parkey            8
027000011204     c                   parm      *blanks       cmd               1
027100011204     c* ritorno senza scelta
027200011204     c                   if        cmd = 'L'
027300011204     c                   movel     parkeys       parkey
027400011204     C                   END
027500011204     C                   END
027600011204     C                   MOVEL     *blanks       tbluni
027700011204     C                   MOVEL     parcod        COD
027800011204     C                   MOVEL(p)  parkey        KEY
027900011204     C     KTAB          CHAIN     TABEL                              99
028000011204     c*
028100011204     C                   ENDSR
028200011204     c**********************************************************************
028300011204     c* pulisce 1° formato
028400011204     c**********************************************************************
028500011204     C     clr00         BEGSR
028600011204     C                   Z-ADD     0             §RIGA
028700011204     C                   Z-ADD     0             §COLON
028800011204     C                   CLEAR                   §FLD
028900011204     C                   ENDSR
029000011204     c**********************************************************************
029100011204     C** ricerca  1 VIDEATA
029200011204     c**********************************************************************
029300011204     C     ric01         BEGSR
029400011204     c                   movel     '1'           fld               1
029500011204     c                   select
029600011204     C*CODICE FILIALE
029700011204     C                   when      §fld = 'VFIL'
029800011204     C                   MOVEL     *BLANKS       §COD1
029900011204     C                   MOVE      *BLANKS       §DES1
030000011204     C                   CALL      'TNSD19R'
030100011204     C                   PARM                    §COD1             3
030200011204     C                   PARM                    §TIP              1
030300011204     C                   PARM                    §DES1            25
030400011204     C                   MOVEL     §COD1         VFIL                           *CODICE
030500011204     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
030600011204     C*CATEGORIA
030700141124     C                   when      §fld = 'VCAT1'
030800141124     c                   z-add     1             i
030900141124     c                   exsr      srvcat
031000141124     C                   when      §fld = 'VCAT2'
031100141124     c                   z-add     2             i
031200141124     c                   exsr      srvcat
031300141124     C                   when      §fld = 'VCAT3'
031400141124     c                   z-add     3             i
031500141124     c                   exsr      srvcat
031600141124     C                   when      §fld = 'VCAT4'
031700141124     c                   z-add     4             i
031800141124     c                   exsr      srvcat
031900141124     C                   when      §fld = 'VCAT5'
032000141124     c                   z-add     5             i
032100141124     c                   exsr      srvcat
032200141124     C                   when      §fld = 'VCAT1'
032300141124     c                   z-add     6             i
032400141124     c                   exsr      srvcat
032500141124     C                   when      §fld = 'VCAT2'
032600141124     c                   z-add     7             i
032700141124     c                   exsr      srvcat
032800141124     C                   when      §fld = 'VCAT3'
032900141124     c                   z-add     8             i
033000141124     c                   exsr      srvcat
033100141124     C                   when      §fld = 'VCAT4'
033200141124     c                   z-add     9             i
033300141124     c                   exsr      srvcat
033400141124     C                   when      §fld = 'VCAT5'
033500141124     c                   z-add     10            i
033600141124     c                   exsr      srvcat
033700011204     C*CODICE AUTISTA
033800011204     C                   when      §fld = 'VAUS'
033900011204     c                   exsr      srvaus
034000011204     c* TARGA
034100011221     C                   WHEN      §FLD = 'VTGN' or §fld = 'VTGA'
034200011221     C                   CALL      'TRMZ02R4'    RICTAR
034300160810     C*CODICE marca
034400160810     C                   when      §fld = 'VCAUT'
034500160810     c                   clear                   tibs02ds
034600160810     c                   move      'R'           t02mod
034700160810     c                   exsr      srvcaut
034800011204     C                   endsl
034900011204     C* POSIZIONAMENTO CURSORE
035000011204     C                   Z-ADD     §RIGA         §RIG
035100011204     C                   Z-ADD     §COLON        §COL
035200160222     c                   clear                   fld
035300011204     c*
035400011204     C                   ENDSR
035500011204     C*----------------------------------------------------*
035600011204     C     AZZER         BEGSR
035700011204     C*----------------------------------------------------*
035800011207     C                   move      *date         dtaiso
035900011207     C                   extrct    dtaiso:*y     ANNO
036000011205     C                   Z-ADD     0             MESE
036100011204     C                   MOVEL     *BLANKS       VSCEF
036200011204     C                   MOVEL     *BLANKS       VSCEC
036300011204     C                   MOVEL     *BLANKS       VSCEA
036400011204     C                   MOVEL     *BLANKS       VSCET
036500011205     C                   z-add     0             VFIL
036600160810     C                   z-add     0             Vkmdal
036700160810     C                   z-add     0             Vkmal
036800160810     C                   MOVEL     *blanks       VCaut
036900160902     C                   MOVEL     *blanks       Vfun
037000141124     C                   MOVEL     *blanks       VCAT
037100141124     C                   clear                   cat
037200011204     C                   MOVEL     *BLANKS       VAUS
037300011204     C                   MOVEL     *BLANKS       VTGA
037400011204     C                   MOVEL     *BLANKS       VTGN
037500011204     C                   MOVEL     *BLANKS       VDFIL
037600011204     C                   MOVEL     *BLANKS       VDAUS
037700011204     C                   ENDSR
037800011204     C*----------------------------------------------------*
037900011204     C     *INZSR        BEGSR
038000011204     C*----------------------------------------------------*
038100011204     C     *ENTRY        PLIST
038200011204     C                   PARM                    KPJBA
038300011204     C                   Z-ADD     1             CODUT
038400011204     C                   CALL      'XPARUT'
038500011204     C                   PARM                    UTEDSE
038600011204     C     CODUT         ifeq      -1
038700011204     c                   seton                                        lr
038800011204     c                   end
038900011204     C                   MOVEL     RAGUT         RSUT             20
039000011204     C                   MOVEL     RAGUT         RSUT2            25
039100011204     C* POSIZIONAMENTO X N.DI TARGA                        *
039200011204     C     KTAR          KLIST                                                  *
039300011204     C                   KFLD                    VTGA                           *
039400011204     C                   KFLD                    VTGN                           *
039500011204     C* RICERCA TARGA X CITTA'
039600011204     C     RICTAR        PLIST
039700011204     C                   PARM                    CODUT
039800011204     C                   PARM                    RSUT
039900011204     C                   PARM                    VTGA
040000011204     C                   PARM                    VTGN
040100011204     C*----------------------------------------------------*
040200011204     C* POSIZIONAMENTO TABELLE                             *
040300011204     C     KTAB          KLIST
040400011204     C                   KFLD                    CODUT
040500011204     C                   KFLD                    COD               2
040600011204     C                   KFLD                    KEY               8
040700011205     c                   endsr
