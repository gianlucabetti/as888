000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - STATISTICA CONSUMI  - FILTRO -                 *
000300000000     H*             --------------------------------                 *
000400000000     H*--------------------------------------------------------------*
000500011204     FTRMZ16D   CF   E             WORKSTN
000600000000     FTABEL00F  IF   E           K DISK
000700011204     FAZORG01L  IF   e           K DISK
000800011204     Fmzame02L  IF   e           K DISK
000801150102     Fmzamh02L  IF   e           K DISK
000900011205     d param           ds
001000011205     d anno                           4  0
001100011205     d mese                           2  0
001200011205     d vscef                          1
001300011205     d vscec                          1
001400011205     d vscea                          1
001500011205     d vscet                          1
001600011205     d vfil                           3  0
001700141124     d vcat                          20
001800011205     d vaus                           2
001900011205     d vtga                           2
002000011205     d vtgn                           9
002100141124     d vvend                          1
002200011207     d dtaiso          s               d
002300141124     D cat             S              2    dim(10) inz
002400000000     D KPJBA         E DS
002500000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002600141124      *
002700141124     iga16d001
002800141124     i              vcat1                       cat(1)
002900141124     i              vcat2                       cat(2)
003000141124     i              vcat3                       cat(3)
003100141124     i              vcat4                       cat(4)
003200141124     i              vcat5                       cat(5)
003300141124     i              vcat6                       cat(6)
003400141124     i              vcat7                       cat(7)
003500141124     i              vcat8                       cat(8)
003600141124     i              vcat9                       cat(9)
003700141124     i              vcat10                      cat(10)
003800000000     C*---------------------------------------------------------------*
003900011204     C                   IF        NOT *INLR
004000011204     C                   EXSR      AZZER
004100011204     C                   do        *hival
004200011204     c                   EXSR      CLR00
004300011204     c                   if        *in99
004400011204     c                   setoff                                       99
004500011204     c                   write     ga16d001
004600011204     c                   seton                                        99
004700011204     c                   end
004800011204     C                   EXFMT     GA16D001
004900011204     C   Kc              leave
005000011204     C* RICERCA
005100011204     C   Kd              EXSR      ric01
005200011204     c* controlli
005300011204     C                   EXSR      ctr01
005400011204     c   99              iter
005500011204     C*CONTROLLI SUPERATI
005600011204     c                   if        *inkf
005700011205     c*
005800141124     c                   movea     cat           vcat
005900011211     C                   MOVEL     'MZ17'        KCOAZ
006000011205     C                   MOVEL(p)  param         kpjbu
006100011204     C                   CALL      'BCH09'
006200011204     C                   PARM                    KPJBA
006300011204     C     KNMUS         IFEQ      *ALL'1'
006400011212     C                   CALL      'TRMZ17C'
006500011204     C                   PARM                    KPJBA
006600011204     C                   ELSE
006700011204     C                   CALL      'BCH10'
006800011204     C                   PARM                    KPJBA
006900011204     C                   END
007000011204     C                   END
007100011204     C                   ENDdo
007200011204     C                   END
007300011204     C                   SETON                                        LR
007400011204     C*----------------------------------------------------*
007500011204     C     CTR01         BEGSR
007600011204     C*----------------------------------------------------*
007700011205     C                   SETOFF                                       99
007800011204     C                   DO
007900011204     C*PERIODO
008000141205     c                   if        mese  =  0
008100011205     c                   if        anno  = 0
008200011204     c                   seton                                        4099
008300011204     c                   leave
008400011204     c                   end
008500011205     c                   if        mese  > 12
008600011204     c                   seton                                        4199
008700011204     c                   leave
008800011204     c                   end
008900011204     c                   end
009000011204     C* ORDINAMENTO
009100000000     C     VSCEF         IFEQ      *BLANKS
009200000000     C     VSCEC         ANDEQ     *BLANKS
009300011204     C     VSCEa         ANDEQ     *BLANKS
009400011204     C     VSCEt         ANDEQ     *BLANKS
009500011204     C                   SETON                                        4599
009600011204     C                   leave
009700000000     C                   END
009800011219     c                   z-add     0             conta             2 0
009900011219     c                   if        vscef <> *blanks
010000011219     c                   add       1             conta
010100011219     c                   end
010200011219     c                   if        vscec <> *blanks
010300011219     c                   add       1             conta
010400011219     c                   end
010500011219     c                   if        vscea <> *blanks
010600011219     c                   add       1             conta
010700011219     c                   end
010800011219     c                   if        vscet <> *blanks
010900011219     c                   add       1             conta
011000011219     c                   end
011100011219     c                   if        conta > 1
011200011219     C                   SETON                                        4599
011300011219     C                   leave
011400011219     c                   end
011500000000     C** FILIALE
011600011205     c                   if        vfil <> 0
011700011204     C                   EXSR      SRVFIL
011800011204     C   99              leave
011900011212     C                   ELSE
012000011212     C                   MOVEL     *BLANKS       VDFIL
012100011204     c                   end
012200141124     C*CATEGORIA da 1 a 10
012300141124     c                   do        10            i                 2 0
012400141124     c                   if        cat(i) <> *blanks
012500141124     c                   exsr      srvCATc
012600011204     C   99              leave
012700011204     c                   end
012800141124     c                   enddo
012900011204     C*CODICE AUTISTA
013000011204     c                   if        vAUS <> *blanks
013100011204     C                   EXSR      SRVAUS
013200011204     C   99              leave
013300011212     C                   ELSE
013400011212     C                   MOVEL     *BLANKS       VDAUS
013500011204     c                   end
013600011204     C*TARGA
013700011204     C     VTGA          IFNE      *BLANKS
013800011204     C     VTGN          ANDEQ     *BLANKS
013900011204     C                   SETON                                        4699
014000011204     C   99              leave
014100011204     c                   end
014200011204     C     VTGA          IFEQ      *BLANKS
014300011204     C     VTGN          ANDNE     *BLANKS
014400011204     C                   SETON                                        4699
014500011204     C   99              leave
014600011204     c                   end
014700011204     C     VTGA          IFNE      *BLANKS
014800011204     C     VTGN          ANDNE     *BLANKS
014900011204     C     KTAR          CHAIN     mzAME02l                           46
014901150102     C   46KTAR          CHAIN     mzAMh02l                           46
015000011219     C   46              SETON                                        99
015100011219     C   99              leave
015200011204     c                   end
015300011204     c                   enddo
015400011204     C                   ENDsr
015500011204     c**********************************************************************
015600011204     C** FILIALE
015700011204     c**********************************************************************
015800011204     C     srvfil        BEGSR
015900011204     c*
016000011205     C     vfil          CHAIN     AZORG
016100011204     C                   if        not %found(azorg01l)
016200011204     C                   SETON                                        4299
016300011204     c                   else
016400011204     C                   MOVEL     ORGDES        VDFIL
016500011204     C                   END
016600011204     c*
016700011204     C                   ENDSR
016800011204     c**********************************************************************
016900011204     C** categoria
017000011204     c**********************************************************************
017100141124     C     srvcat        BEGSR
017200011204     c*
017300011204     c                   movel     'XA'          parcod            2
017400141124     c                   movel(P)  CAT(i)        parkey
017500011204     c                   exsr      srtab
017600141124     c                   movel     parkey        cat(i)
017700011204     c                   if        *in99
017800141124     c                   z-add     49            ii                2 0
017900141124     c                   add       i             ii
018000141124     C                   eval      *in(ii) = *on
018100011204     C                   END
018200011204     c*
018300011204     C                   ENDSR
018400141124     c**********************************************************************
018500141124     C** categoria solo controllo
018600141124     c**********************************************************************
018700141124     C     srvcatc       BEGSR
018800141124     c*
018900141124     c                   movel     'XA'          cod
019000141124     c                   movel(P)  CAT(i)        key
019100141124     C     KTAB          CHAIN     TABEL                              99
019200141124     c                   if        *in99
019300141124     c                   z-add     49            ii                2 0
019400141124     c                   add       i             ii
019500141124     C                   eval      *in(ii) = *on
019600141124     C                   END
019700141124     c*
019800141124     C                   ENDSR
019900011204     c**********************************************************************
020000011204     C** autista
020100011204     c**********************************************************************
020200011204     C     srvaus        BEGSR
020300011204     c                   movel     'XN'          parcod
020400011204     c                   movel(P)  Vaus          parkey
020500011204     c                   exsr      srtab
020600011204     c                   movel     parkey        vaus
020700011204     c                   if        *in99
020800011204     C                   MOVEL     *blanks       VDAUs
020900011204     C                   SETON                                        44
021000011204     C                   else
021100011204     C                   MOVEL     TBLUNI        VDAUs
021200011204     C                   END
021300011204     c*
021400011204     C                   ENDSR
021500011204     C**********************************************************************
021600011204     C** Controlli e decodifiche campi con tabel00f
021700011204     C**********************************************************************
021800011204     C     srtab         BEGSR
021900011204     c                   if        fld <> *blanks
022000011204     c                   movel     parkey        parkeys           8
022100011204     C                   CALL      'TRUL19R'
022200011204     c                   parm                    parcod            2
022300011204     c                   parm      '1'           parord            1
022400011204     c                   parm      *blanks       parkey            8
022500011204     c                   parm      *blanks       cmd               1
022600011204     c* ritorno senza scelta
022700011204     c                   if        cmd = 'L'
022800011204     c                   movel     parkeys       parkey
022900011204     C                   END
023000011204     C                   END
023100011204     C                   MOVEL     *blanks       tbluni
023200011204     C                   MOVEL     parcod        COD
023300011204     C                   MOVEL(p)  parkey        KEY
023400011204     C     KTAB          CHAIN     TABEL                              99
023500011204     c*
023600011204     C                   ENDSR
023700011204     c**********************************************************************
023800011204     c* pulisce 1° formato
023900011204     c**********************************************************************
024000011204     C     clr00         BEGSR
024100011204     C                   Z-ADD     0             §RIGA
024200011204     C                   Z-ADD     0             §COLON
024300011204     C                   CLEAR                   §FLD
024400011204     C                   ENDSR
024500011204     c**********************************************************************
024600011204     C** ricerca  1 VIDEATA
024700011204     c**********************************************************************
024800011204     C     ric01         BEGSR
024900011204     c                   movel     '1'           fld               1
025000011204     c                   select
025100011204     C*CODICE FILIALE
025200011204     C                   when      §fld = 'VFIL'
025300011204     C                   MOVEL     *BLANKS       §COD1
025400011204     C                   MOVE      *BLANKS       §DES1
025500011204     C                   CALL      'TNSD19R'
025600011204     C                   PARM                    §COD1             3
025700011204     C                   PARM                    §TIP              1
025800011204     C                   PARM                    §DES1            25
025900011204     C                   MOVEL     §COD1         VFIL                           *CODICE
026000011204     C                   MOVEL     §DES1         VDFIL                          *DESCRIZIONE
026100011204     C*CATEGORIA
026200141124     C                   when      §fld = 'VCAT1'
026300141124     c                   z-add     1             i
026400141124     c                   exsr      srvcat
026500141124     C                   when      §fld = 'VCAT2'
026600141124     c                   z-add     2             i
026700141124     c                   exsr      srvcat
026800141124     C                   when      §fld = 'VCAT3'
026900141124     c                   z-add     3             i
027000141124     c                   exsr      srvcat
027100141124     C                   when      §fld = 'VCAT4'
027200141124     c                   z-add     4             i
027300141124     c                   exsr      srvcat
027400141124     C                   when      §fld = 'VCAT5'
027500141124     c                   z-add     5             i
027600141124     c                   exsr      srvcat
027700141124     C                   when      §fld = 'VCAT1'
027800141124     c                   z-add     6             i
027900141124     c                   exsr      srvcat
028000141124     C                   when      §fld = 'VCAT2'
028100141124     c                   z-add     7             i
028200141124     c                   exsr      srvcat
028300141124     C                   when      §fld = 'VCAT3'
028400141124     c                   z-add     8             i
028500141124     c                   exsr      srvcat
028600141124     C                   when      §fld = 'VCAT4'
028700141124     c                   z-add     9             i
028800141124     c                   exsr      srvcat
028900141124     C                   when      §fld = 'VCAT5'
029000141124     c                   z-add     10            i
029100141124     c                   exsr      srvcat
029200011204     C*CODICE AUTISTA
029300011204     C                   when      §fld = 'VAUS'
029400011204     c                   exsr      srvaus
029500011204     c* TARGA
029600011221     C                   WHEN      §FLD = 'VTGN' or §fld = 'VTGA'
029700011221     C                   CALL      'TRMZ02R4'    RICTAR
029800011204     C                   endsl
029900011204     C* POSIZIONAMENTO CURSORE
030000011204     C                   Z-ADD     §RIGA         §RIG
030100011204     C                   Z-ADD     §COLON        §COL
030101160222     c                   clear                   fld
030200011204     c*
030300011204     C                   ENDSR
030400011204     C*----------------------------------------------------*
030500011204     C     AZZER         BEGSR
030600011204     C*----------------------------------------------------*
030700011207     C                   move      *date         dtaiso
030800011207     C                   extrct    dtaiso:*y     ANNO
030900011205     C                   Z-ADD     0             MESE
031000011204     C                   MOVEL     *BLANKS       VSCEF
031100011204     C                   MOVEL     *BLANKS       VSCEC
031200011204     C                   MOVEL     *BLANKS       VSCEA
031300011204     C                   MOVEL     *BLANKS       VSCET
031400011205     C                   z-add     0             VFIL
031500141124     C                   MOVEL     *blanks       VCAT
031600141124     C                   clear                   cat
031700011204     C                   MOVEL     *BLANKS       VAUS
031800011204     C                   MOVEL     *BLANKS       VTGA
031900011204     C                   MOVEL     *BLANKS       VTGN
032000011204     C                   MOVEL     *BLANKS       VDFIL
032100011204     C                   MOVEL     *BLANKS       VDAUS
032200011204     C                   ENDSR
032300011204     C*----------------------------------------------------*
032400011204     C     *INZSR        BEGSR
032500011204     C*----------------------------------------------------*
032600011204     C     *ENTRY        PLIST
032700011204     C                   PARM                    KPJBA
032800011204     C                   Z-ADD     1             CODUT
032900011204     C                   CALL      'XPARUT'
033000011204     C                   PARM                    UTEDSE
033100011204     C     CODUT         ifeq      -1
033200011204     c                   seton                                        lr
033300011204     c                   end
033400011204     C                   MOVEL     RAGUT         RSUT             20
033500011204     C                   MOVEL     RAGUT         RSUT2            25
033600011204     C* POSIZIONAMENTO X N.DI TARGA                        *
033700011204     C     KTAR          KLIST                                                  *
033800011204     C                   KFLD                    VTGA                           *
033900011204     C                   KFLD                    VTGN                           *
034000011204     C* RICERCA TARGA X CITTA'
034100011204     C     RICTAR        PLIST
034200011204     C                   PARM                    CODUT
034300011204     C                   PARM                    RSUT
034400011204     C                   PARM                    VTGA
034500011204     C                   PARM                    VTGN
034600011204     C*----------------------------------------------------*
034700011204     C* POSIZIONAMENTO TABELLE                             *
034800011204     C     KTAB          KLIST
034900011204     C                   KFLD                    CODUT
035000011204     C                   KFLD                    COD               2
035100011204     C                   KFLD                    KEY               8
035200011205     c                   endsr
