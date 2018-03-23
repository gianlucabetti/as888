000100020108      /TITLE Pulizia stime
000300011221     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000400930111     H DECEDIT('0,') DATEDIT(*DMY.)
000800020108     FYnStA00F  IF   E           K DISK    usropn
002004020107     D xchkautds     e ds                  inz
002100020108     D XSoc001DS     E DS                  inz
002101020108     D ynsti         e DS                  extname(ynsti00f)
002700891030     D KPJBA         E DS
002702020108     D  MSGERR               453    502
002703020108     D WrkSqlCmd       S           1024
002704020108     D anno            S                   like(stiann) inz
010000011221
042000921223     C     *ENTRY        PLIST                                                  *
042100921223     C                   PARM                    KPJBA                          *
042300011219     ** Mi collego ad una società.
042400011219     C                   CALLB     'XSOC'
042500011219   >>C                   PARM      'SOC001'      TipXsc            6
042600011219   >>C                   PARM      *BLANK        SocXsc            3
042700011219   >>C                   PARM      *ZERO         CdsXsc            9 0
042800011219   >>C                   PARM      *BLANK        ModXsc            3
042900011219   >>C                   PARM      *BLANK        RtnXsc            1
043000011219     C                   PARM                    XSoc001DS
043100011219   >>C                   PARM                    Kpjba
043300011219     ** Collegamento fallito.
043400011219     C                   IF        RtnXsc = '1'
043500011219     C                   EVAL      MsgErr =
043600011219     C                             'Non è possibile collegarsi alla società '
043700011219     C                             + XscSoc + '.'
043800011219     C                   EVAL      *INLR = *ON
043900011219     C                   RETURN
044000011219     C                   ENDIF
044800011219     ** Inserire qui un eventuale controllo utente autorizzato.
044801020108     C                   CLEAR                   XChkAutDS
044802020108     C                   EVAL      XcaVfu = 'STIME'
044803020108     C                   EXSR      ChkAut
044804020108     C                   IF        XcaErr = '1'
044805020108     C                   exsr      uscita
044806020108     C                   ENDIF
045000011219     ** Reperisco parametri generali.
045100011219     C                   OPEN      YnStA00F
045200011219     C                   READ      YnStA00F
045300011219     C                   CLOSE     YnStA00F
045400011219     C                   IF        %EOF
045500011219     C                   EVAL      MsgErr =
045600011219     C                             'Mancano i parametri nel file YNSTA00F.'
045700020108     C                   exsr      uscita
045900011219     C                   ENDIF
045901020108     c* ELIMINO STIME DELL'ANNO PRECEDENTE
046400020108     C                   eval      anno = StaAnn - 1                            *
046401020108     C                   EVAL      WrkSqlCmd
046402020108     C                             =
046403020108     C                             'DELETE '
046404020108     C                             +
046406020108     C                             'FROM YNSTI00F '
046407020108     C                             +
046409020108     C                             'WHERE STIANN = ' + %editc(anno:'Z')
046410020108     C                             +
046411020108     C                             ' AND STISOCIETA = ' + '''' + xscsoc + ''''
046412020108     C/EXEC SQL
046413020108     C+ EXECUTE IMMEDIATE :WrkSqlCmd
046414020108     C/END-EXEC
046415020108
046416020108     C                   IF        SqlCod <> 0
046417020108     C                             AND
046418020108     C                             SqlCod <> 100
046420020108     C                   CALL      'X66CHGJOB'
046421020108     C                   EXSR      Uscita
046422020108     C                   ENDIF
046459020108     C                   SETON                                        LR
051301020104     C******************************************************
051302020108     c*      Reperisco autorizzazioni per l'uso del pgm
051303020108     C     chkaut        BEGSR
051304020104     C******************************************************
051307020108     C                   EVAL      XcaSoc = XscSoc
051308020108     C                   EVAL      XcaGrp = XscGrp
051309020225     C                   EVAL      XcaFnc = 'YSTIM2'
051311020108     C                   EVAL      XcaTck = 'CK'
051312020108     C                   EVAL      XcaCut = XscCut
051313020108     C                   EVAL      XcaNau = '1'
051315020108     C                   CALLB     'XCHKAUT'
051316020108     C                   PARM                    XChkAutDS
051318020108     C                   ENDSR
051600011221     ***********************************************************************
051800011221     ** Uscita dal programma.
052000011221     ***********************************************************************
052100011221     C     Uscita        BEGSR
052300011221     C                   EVAL      *INLR = *ON
052400011221     C                   RETURN
052600011221     C                   ENDSR
