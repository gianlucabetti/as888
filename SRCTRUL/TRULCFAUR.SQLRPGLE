000200171212     H DEBUG DECEDIT('0,') DATEDIT(*ymd)
000300000000     H*--------------------------------------------------------------*
000400171212     ffiapd41l  if   e           k disk
000401171212     faitra03l  if   e           k disk
000800171212      * recupera numero cellulare su cf corrispondente ma di altra anagrafica
000801171212     D kpjba         e DS
000900020624      *
000901171212     D dapd4tel      e DS
001000020624     D param           DS
001100171212     D  Icodaut                       7s 0
001101171212     D  Ocodaut                       7s 0
001102171212     D  Onumcel                      16
001200120216     d  esito                         1
001201171212      *
001202171212     D  codaut         s              7s 0
001203171212     D  tipa           s              1
001204171212     D  trd            s              3
001205171212     D  wdate          s              8  0
001206171213     D  cfisc          s             16
001405120502     C*Legenda errori possibili di ritorno
001406171212     C* esito <> '1' non trovato numero di cellulare
001410120502      * ?_______________________________________________________________________
001500020624     c     *entry        plist
001501171212     c                   parm                    kpjba
001600171212     c                   movel     kpjbu         param
001601171212     c                   clear                   Ocodaut
001602171212     c                   clear                   Onumcel
001603171212     c                   clear                   esito
001604171212      *
001605171212     c                   move      *date         wdate
002300120306     c*
002301171212     c     kap4          klist
002302171212     c                   kfld                    TIPA
002303171212     c                   kfld                    codaut
002304171212     c                   kfld                    TRD
002305171212     c                   move      'A'           tipa
002307171212     c                   move      'TEL'         TRD
002308171212      *
002900171212     c     icodaut       setll     aitra03l
002901171212     c                   do        *hival
002902171212     c     icodaut       reade     aitra03l
003100171212     c                   if        %eof(aitra03l)
003200171212     c                   return
003300171212     c                   endif
003301171213      * non controllo accreditamento valido ma se presente C.F.
003302171213      * nel caso in caso di disaccreditamento non venisse aggiornato il record sul fiapd4
003303171213      * bisogna controllare con la specifica asteriscata e non con il solo C.F
003400171213     c*                  if        tradfi = 0 or tradfi > Wdate
003401171213     c                   if        tracf <> *blank
003500171213     c                   movel     tracf         cfisc
003501171212     c                   leave
003600171212     c                   endif
003601171212     c                   enddo
003700120306     c*
004400020313     C/EXEC SQL
004500171212     C+ DECLARE A1 CURSOR FOR SELECT trakau
004600171212     C+ FROM aitra00f WHERE tracf = :cfisc
004700171212     C+ Group by trakau
004900020313     C/END-EXEC
005000030109      *
005001171212     C/EXEC SQL
005002171212     C+ OPEN A1
005003171212     C/END-EXEC
005004171212     C                   DOU       SqlCod <> 0
005005171212     C/EXEC SQL
005006171212     C+ FETCH NEXT FROM A1 INTO :codaut
005007171212     C/END-EXEC
005008171212     C                   SELECT
005009171212     c*
005010171212     C                   WHEN      SqlCod = 0
005011171212     c     kap4          setgt     fiapd41l
005012171212     c                   do        *hival
005013171212     c     kap4          readpe    fiapd41l
005014171212     c                   if        %eof(fiapd41l)
005015171212     c                   leave
005016171212     c                   endif
005017171212     c                   movel     apd4uni       dapd4tel
005018171212     c                   if        §APD4CEL <> *blank
005020171212     c                   z-add     codaut        Ocodaut
005021171212     c                   movel     §apd4cel      Onumcel
005022171212     c                   move      '1'           esito
005023171212     c                   movel     param         kpjbu
005024171212     c                   leave
005025171212     c                   endif
005026171212     c                   enddo
005027171212     C*
005028171212      *   EoF
005029171212     C                   WHEN      SqlCod = 100
005030171212     c                   leave
005031171212      **
005032171212      *   Errori
005033171212     C                   WHEN      SqlCod <> 0
005034171212     c                   leave
005035171212      **
005036171212     C                   ENDSL
005037171212      **
005038171212     C                   ENDDO
005039171212     C/EXEC SQL
005040171212     C+ CLOSE A1
005041171212     C/END-EXEC
005500020624      *
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
