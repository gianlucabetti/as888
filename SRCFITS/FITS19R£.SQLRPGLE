000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040913      * FITS18R  *----------------------------------------------------*
000400040913      *  giacenza partenza : GCP GAP GNP
000500021010      *---------------------------------------------------------------* c
000600060201     ftiGCPNEW  if A e           k disk    rename(tiGCP000:GCPNEW)
000700060201     ftignpNEW  o  A e           k disk    rename(tignp000:GnPNEW)
000800060201     ftigapNEW  o  A e           k disk    rename(tigap000:GaPNEW)
000900040913     fFnblp01l  If   e           k disk
001000060201     fFnarb01l  If   e           k disk
001100060201     ftignp01l  If   e           k disk
001200060201     ftigap01l  If   e           k disk
001300040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400021008      *
001500060201     d tigcpds       E DS                  extname(tigcp00f)
001600060201     d KPJBA         E DS
001700040805     D* Schiera p.o. da trasferire
001800040805     D  SKPO           s              3    dim(500)
001900040913     d KLIN            s                   like(BLPLNP)
002000040805      *
002100021008      *****************************************************************
002200021010     c     *ENTRY        PLIST
002300021010     c                   PARM                    KPJBA
002400040805     C                   parm                    PARAM          1500
002500040805     c
002600040805     c     ' ':'0'       xlate     param         param
002700040805     c                   movea     param         skpo
002800040805     C*
002900040913     C     Kbol          klist
003000040913     C                   kfld                    gcpaas
003100040913     C                   kfld                    gcplnp
003200040913     C                   kfld                    gcpnrs
003300040913     C                   kfld                    gcpnsp
003400040913     C     KGCP          klist
003500040913     C                   kfld                    gcpagc
003600040913     C                   kfld                    gcpfgc
003700040913     C                   kfld                    gcpngc
003800040913     C                   kfld                    gcpfrg
003900021010      *---------------------------------------------------------------*
004000021008      ****
004100040910      ** LETTURA
004200021008      ****
004300040913     c                   z-add     1             x                 4 0
004400040913    1c                   dow       skpo(x)>*zeros
004500040913     c                   movel     skpo(X)       klin
004600060201     C/EXEC SQL
004700060201     C+ DECLARE E1 CURSOR FOR SELECT * FROM tigcp00f WHERE GCPLNP = :klin
004800060209     C+ or GCPLNa = :klin or GCPLNpa= :klin or GCPLNaa = :klin and
004900060209     C+ GCPSEDE = ' '
005000060201     C/END-EXEC
005100060201
005200060201     C/EXEC SQL
005300060201     C+ OPEN E1
005400060201     C/END-EXEC
005500060201     C                   DO        *hival
005600060201     C/EXEC SQL
005700060201     C+ FETCH NEXT FROM E1 INTO :tigcpds
005800060201     C/END-EXEC
005900060201     C                   SELECT
006000060201     c*
006100060201     C                   WHEN      SqlCod = 100
006200060201     c                   leave
006300060201     C                   WHEN      SqlCod < 0
006400060201     c                   leave
006500060201     c                   other
006600060201     C                   EXSR      ELABORA
006700060201     C                   ENDSL
006800060201      *
006900060201     C                   ENDDO
007000060201     C/EXEC SQL
007100060201     C+ CLOSE E1
007200060201     C/END-EXEC
007300040913     C                   ADD       1             X
007400040913     C                   ENDDO
007500021010      *
007600040805     c                   EXCEPT    RINUM1
007700021011     c                   seton                                        LR
007800040913      *---------------------------------------------------------------*
007900040913     c*    elabora i dati
008000040913      *---------------------------------------------------------------*
008100040913     c     ELABORA       begsr
008200060201     c                   do
008300040913      * testo se esiste gi� nelle giacenze trascodificate se la trovo
008400040913      * stampo l'anomalia
008500060201     C     KgCP          chain     tigcpNEW
008600060201     C                   if        NOT %found(tigcpNEW)
008700060201      * DEVE ESISTERE LA BOLLA in partenza o in arrivo
008800040913     C     Kbol          chain     FNblp01l
008900040913     C                   if        not %found(fnblp01l)
009000060201     C     Kbol          chain     FNarb01l
009100060201     C                   if        not %found(fnarb01l)
009200040913     C                   ADD       1             TOTNOBOLLA        7 0
009300060201     c                   leave
009400060201     c                   end
009500060201     c                   end
009600060201     c*
009700040913     c                   write     gcpnew
009800040913     c                   add       1             totgcp            7 0
009900040913     c* elaboro i file secondari
010000040913     C* note
010100060201     C     Kgcp          SETLL     tignp01L
010200060201     C     Kgcp          READE     tignp01L
010300060201     C                   dow       not %eof(tignp01l)
010400040913     c                   write     gnpnew                               99
010500040913     c                   add       1             Totgnp            7 0
010600060201     C     Kgcp          READE     tignp01L
010700040913     c                   enddo
010800040913     C* anagrafica mittente
010900060201     C     Kbol          chain     tigap01L
011000060201     C                   if        %found(tigap01l)
011100040913     c                   write     gapnew                               99
011200040913     c  n99              add       1             Totgap            7 0
011300040913     c                   endif
011400040913     c
011500040913     C                   ENDIF
011600060201     C                   ENDdo
011700040913     C                   ENDSR
011800021009      *
011900021010      *---------------------------------------------------------------*
012000040805     oQSYSPRT   E            RINUM1         2 02
012100040913     o                                         +  5 'GIACENZE PARTENZA'
012200040913     o                                         +  5 'Fits18R'
012300021010     o                       UDATE             +  2 '  /  /  '
012400021010     o          E            RINUM1      1
012500060201     o                                         +  3 'TIGCP00F:'
012600040913     o                       TOTgcp        2   +  2
012700060201     o                                         +  3 'TIGNP00F:'
012800040913     o                       TOTgNP        2   +  2
012900060201     o                                         +  3 'TIGAP00F:'
013000040913     o                       TOTgAp        2   +  2
013100040913     o                                         +  3 'giacenze di cui no bolla:'
013200040913     o                       TOTnobolla    2   +  2
