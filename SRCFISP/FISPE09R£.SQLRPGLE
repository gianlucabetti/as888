000100080605     h dftactgrp(*no) actgrp(*caller)
000200060918      //********************************************************************************************
000300060918      //
000400060918      // Definizione variabili.
000500060918      //
000600060918      //********************************************************************************************
000700131009     Ftabel00f  IF   E           k DISK
000701131009     Ftntbe01l  IF   E           k DISK
000702131009     Fazlin03l  IF   E           k DISK
000703131009
000704131009     d ICCM            s              7s 0
000705131009     d Ilna            s              3s 0
000706131009     d INzd            s              3a
000707131009     d Iprd            s              2a
000708131009     d Icad            s              9a
000709131009     d Ilod            s             35a
000710131009     d Iind            s             35a
000711131009     d Irsd            s             35a
000712131009     d Ilang           s              2a
000713131009
001000061017     D RTNesito        S             10I 0
001100061017     D RTNopcode       S             10
001200061017     D RTNstatus       S             10I 0
001201131009
001400131009     D RIC_tic         S             10I 0 inz
001500061025
001600131009     D DSTIC           DS                  occurs(200) inz
001700131009     D  DSTIC_cod                     2A
001800131009     D  DSTIC_deco                   35A
002000061025
002600061025     d xx              s              3  0 inz
002601131009
002602131009     d §kut            s                   like(tblkut) inz(1)
002603131009     d COD             s                   like(tblcod)
002604131009     d wlintntbe       s                   like(lintntbe)
002605131009     d trulTICDS     e ds
002606131009     d tibs02ds      e ds
002607131009     d kpjba         e ds
002608131009     d ds1a          e ds
002609131009     d di1a          e ds
002610131009     d dged          e ds
002700060918      //********************************************************************************************
002800060918      //
002900060918      // Definizione parametri procedura.
003000060918      //
003100060918      //********************************************************************************************
003200060918     C     *ENTRY        PLIST
003300131009     C                   PARM                    ICCM
003301131009     C                   PARM                    Ilna
003302131009     C                   PARM                    INzd
003303131009     C                   PARM                    Iprd
003304131009     C                   PARM                    Icad
003305131009     C                   PARM                    Ilod
003306131009     C                   PARM                    Iind
003307131009     C                   PARM                    Irsd
003308131009     C                   PARM                    Ilang
003500061017     C                   PARM                    rtnesito
003600061017     C                   PARM                    rtnopcode
003700061017     C                   PARM                    rtnstatus
003800061025     c*
003900131009     C     KTAB          KLIST
004000131009     C                   KFLD                    §kut
004100131009     C                   KFLD                    COD
004200060918      /FREE
004300060918
004400060918       //*******************************************************************************************
004500060918       //
004600060918       // Main.
004700060918       //
004800060918       //*******************************************************************************************
004900061017       CLEAR rtnesito;
005000061017       CLEAR rtnopcode;
005100061017       CLEAR rtnstatus;
005200061026       *INLR = *ON;
005300080605
005400080605       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005500060918
005600060918      /END-FREE
005700061025
005701131009     c                   if        ilang=*blanks
005702131009     c                   eval      ilang='it'
005703131009     c                   endif
006000061025
006017131009     c                   exsr      cartab
011900131009     c                   exsr      elabora
012000061025
012100060918     C/EXEC SQL
012200061026     C+ SET RESULT SETS FOR RETURN TO CLIENT
012300131009     c+                 array :DStic  for :RIC_tic rows
012500061026     C/END-EXEC
012600060918      /FREE
012700061017
012800061016       IF sqlCode < 0;
012900061017         RTNstatus = sqlCode;
013000061017         RTNopcode = 'SQLCODE';
013100061017         RTNesito  = -1;
013200060918         RETURN;
013300060918       ENDIF;
013400060918
013500060918       RETURN;
013600060918
013700060918      /END-FREE
013701131009     c**********************************************************************
013702131009     c     cartab        begsr
013703131009      * AGGANCIO TABELLA GED PER DIVISA DELLA MONETA DI CONTO
013704131009     C                   CLEAR                   tibs02ds
013705131009     C                   MOVEL     'C'           T02MOD
013706131009     C                   MOVEL     'GED'         T02COD
013707131009     C                   MOVEL     '1'           T02KE1
013708131009      *
013709131009     C                   CALL      'TIBS02R'
013710131009     C                   PARM                    KPJBA
013711131009     C                   PARM                    tibs02ds
013712131009      *
013713131009     C     T02ERR        IFEQ      *BLANKS
013714131009     C                   MOVEL     T02UNI        DGED
013715131009     C                   ELSE
013716131009     C                   CLEAR                   DGED
013717131009     C                   ENDIF
013718131009     c*
013719131009     c* cerco codice lingua per chain tntbe
013720131009     c                   clear                   wlintntbe
013721131009     c     ilang         chain     azlin03l
013722131009     c                   if        %found(azlin03l)
013723131009     c                   eval      wlintntbe=lintntbe
013724131009     c                   endif
013725131009     c                   endsr
013800061025     c**********************************************************************
013900131009     c     elabora       begsr
016300061027
016301131009     c                   clear                   ric_tic
016302131009     c                   eval      cod='1A'
016303131009     c                   z-add     1             §kut
016304131009     c     ktab          setll     tabel00f
016305131009     c     ktab          reade     tabel00f
016700131009     c                   Dow       not %eof(tabel00f)                           *record letto
016701131009     c                   if        tblflg=*blanks
016702131009     c                   movel     tbluni        ds1a
016703131009     c                   clear                   trulticds
016704131009     c                   eval      iticTBO='P'
016707131009     c                   eval      iticlna=ilna
016708131009     c                   eval      iticnzd=inzd
016709131009     c                   eval      iticprd=iprd
016710131009     c                   eval      iticcad=icad
016711131009     c                   eval      iticlod=ilod
016712131009     c                   eval      iticind=iind
016713131009     c                   eval      iticrsd=irsd
016714131009     C                   eval      iticccm=iccm
016715131009     c                   eval      itictic=%subst(tblkey:1:2)
016716131009     c* forzo passaggio divisa "EUR" per non avere errori riguradanti la
016717131009     c* divisa dal momento che richiamo solo per il tipo incasso c/a
016718131009     c                   eval      iticdiv=§gedcn
016719131010     c                   eval      iticpgm='FNLS01R'
016720131009     c                   call      'TRULTICR'
016721131009     c                   parm                    TRULTICDS
016722131009     c                   parm                    ds1a
016723131009     c                   if        oticerr=*blanks
016724131009     c* reperisco descrizione
016725131009     c                   clear                   tibs02ds
016726131009     c                   clear                   di1a
016727131009     C                   MOVEL     'C'           T02MOD
016728131009     C                   MOVEL     wlintntbe     T02lin
016729131009     C                   MOVEL     'I1A'         T02COD
016730131009     C                   MOVEL(p)  itictic       T02KE1
016731131009     c                   call      'TIBS02R'
016732131009     c                   parm                    kpjba
016733131009     c                   parm                    tibs02ds
016734131009     c                   if        t02err=*blanks
016735131009     C                   MOVEL     T02UNI        Di1a
016736131009     c                   endif
016737131009     c                   add       1             ric_tic
016738131009     c     ric_tic       occur     dstic
016739131009     c                   move      itictic       dstic_cod
016740131009     c                   movel     descrizion    dstic_deco
016741131009     c                   endif
016742131009     c                   endif
016743131009     c     ktab          reade     tabel00f
016744131009     c                   enddo
016745131009     c                   endsr
