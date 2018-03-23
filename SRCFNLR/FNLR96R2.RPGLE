000100950208     H DECEDIT('0,') DATEDIT(*DMY.)
000200060404      * FNLR96R *----------------------------------------------------*
000300911119      *-----------------*                                            *
000400060405      * reperisce motivo variazione
000500911119      *--------------------------------------------------------------*
001801060404     FFiar501L  IF   E           K DISK
002101060404     FTntbe01l  IF   E           K DISK
005901060405     D fnlr96ds2     E DS
005902060405     D dar5mtv       E DS
005903060404     D dmtv          E DS
006000900514     D KPJBA         E DS
006001060405     C     *ENTRY        PLIST
006002060405     C                   PARM                    KPJBA
006003060405     c                   movel(p)  kpjbu         fnlr96ds2
006004060405     c*
006005060405     c                   clear                   d962mtv
006006060405     c                   clear                   d962des
006007060405     c                   clear                   d962txt
006008060405     c                   if        d962tel = ' '
008405060405     c                   clear                   dar5mtv
008406060404     c                   clear                   dmtv
008407060405     c                   eval      ar5trd = '§' + d962cvb
008408060404     C     Kar5          CHAIN     Fiar501L
008409060404     c                   if        %found(fiar501l)
008410060404     c                   eval      dar5mtv = ar5uni
008411060405     c                   eval      d962mtv=§ar5mtv
008412060405     c                   eval      d962txt=§ar5txt
008413060404     c                   exsr      srdecmtv
008414060404     c                   end
008415060405     C                   SETON                                        RT
029400921029     C*
029401060405     c                   else
029500910521     C                   SETON                                        LR
029503060405     c                   end
029504060405     c                   movel     fnlr96ds2     kpjbu
029700911121      *---------------------------------------------------------------*
029800060404     C     srdecmtv      BEGSR
029801060404      *---------------------------------------------------------------*
029802060404     c                   eval      tbecod = 'MTV'
029803060404     c                   eval      tbeke1 = §ar5mtv
029804060404     c     ktbe          chain     tntbe01l
029805060404     c                   if        %found(tntbe01l) and tbeatb = ' '
029806060404     c                   eval      dmtv = tbeuni
029807060405     c                   eval      d962des=§mtvdes
029808060404     c                   end
029809060404     c                   endsr
029810060404      *---------------------------------------------------------------*
029811060404     C     *INZSR        BEGSR
029812060404      *---------------------------------------------------------------*
030100900516      *
038601060404     C     KTbe          KLIST
038602060404     C                   KFLD                    tbecod
038603060404     C                   KFLD                    tbeke1
039900941129     C*
039901060404     C     Kar5          KLIST
039902060405     C                   KFLD                    d962AAS
039903060405     C                   KFLD                    d962LNP
039904060405     C                   KFLD                    d962NRS
039905060405     C                   KFLD                    d962NSP
039906060405     C                   KFLD                    aR5trd
039907060405     C                   KFLD                    d962DTV
039908060405     C                   KFLD                    d962ORV
040000060405     c                   endsr
