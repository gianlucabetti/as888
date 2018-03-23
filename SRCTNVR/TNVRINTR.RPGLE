000100970910     H*------------------------------------------------------------------------*
000200970910     H DECEDIT('0,') DATEDIT(*DMY.)
000300970910     F*------------------------------------------------------------------------*
000400970910     F* DATA BASE
000500970910     F*------------------------------------------------------------------------*
000600151130     Finticom_01if   E             disk    rename(inticom_01:inticom000)
000700151130     Ffnblp01l  uf   E           k disk
000800151130     Ffnarb01l  uf   E           k disk
000900141201     Ftitas30c  uf   E           k disk    extfile(wlibsede) usropn
001000141201     Ftabel00f  if   E           k disk
001100151130     Ffnlbl02l  if   E           k disk
001200141202     fqsysprt   o    F  132        printer
001300060421     d
001400970910     D KPJBA         E DS
001500060421     d Azuteds       e ds                  extname(Azute00f)
001600060421     d dDatiute      e ds
001700141201     d ds3a          e ds
001800060421     D TIBS34DS      E DS
001900110504     d Dataiso         s               d   datfmt(*iso)
002000110504     d Dataeur         s               d   datfmt(*eur)
002100110504     d amgdri          s              8  0
002200141201     d amgprima        s              8  0
002300141126     d wdat            s              8  0
002400141202     d wdata           s              8  0
002500141126     d amgdat          s              8  0
002600141126     d dateu           s              8  0
002700141202     d contagg         s              5  0 inz
002800151130     d contaerr        s              5  0 inz
002900141126     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003000141201     D Wlibsede        S             21
003100141201     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS30C')
003200141201     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS30C')
003300060421     d
003400141201     ITITAS000      02
003500141201     ITITAS010      03
003600141201     ITITASP00      04
003700970910     C*------------------------------------------------------------------------*
003800151130     c                   exsr      elabora
003900151130     c                   except    tot
004000151130     c                   seton                                        lr
004100970910     C*
004200970910     C*------------------------------------------------------------------------*
004300970910     C* *INZSR - OPERAZIONI INIZIALI
004400970910     C*------------------------------------------------------------------------*
004500970910     C     *INZSR        BEGSR
004600970910     C*---
004700970910     C* RICEVIMENTO PARAMETRI
004800970910     C*---
004900970910     C     *ENTRY        PLIST
005000970612     C                   PARM                    KPJBA
005100060421     c
005200110504     c                   z-add     1             codut             1 0
005300060421      *
005400060421     c     *dtaara       define    §azute        azuteds
005500060421     c     *dtaara       define    §datiute      ddatiute
005600060421     c                   in(E)     *dtaara
005700060421    1c                   If        %error  or RSUT = *blanks
005800060421     c                   Clear                   Tibs34ds
005900060421     c                   Call      'TIBS34R'
006000060421     c                   Parm                    Tibs34ds
006100060421     c                   In        *dtaara
006200060421    1c                   EndIf
006300060421     c                   ENDSR
006400141201     C*------------------------------------------------------------------------*
006500141201     C     ELABORA       BEGSR
006600141201      /free
006700141201       if %subst(knsif:7:1) = 'P';
006800141201         Wlibsede = Wlibsedep;
006900141201         Else ;
007000141201         wlibsede = Wlibsedeb;
007100141201       endif;
007200141201       Open   Titas30c;
007300141201      /end-free
007400141204     c
007500141201     c     kblp          klist
007600151130     c                   kfld                    kaas              4 0
007700151130     c                   kfld                    lnp
007800151130     c                   kfld                    nrs
007900151130     c                   kfld                    nsp
008000151130     c                   z-add     2015          kaas
008100151130     c     ktab          klist
008200151130     c                   kfld                    codut             1 0
008300151130     c                   kfld                    kcod              2
008400151130     c                   kfld                    kkey              8
008500141201     c
008600141201     C* LEGGO LE BOLLE PARTENZA DEL CLIENTE
008700151130     c                   read      inticom_01
008800141201     c
008900151130     c                   dow       not %eof(inticom_01)
009000151130     c     kblp          chain     fnblp01l
009100151130     c                   if        not %found(fnblp01l)
009200151130     c                   except    notr
009300151130     c                   else
009400151130     c
009500141201     c* escludo bolle recupero
009600141201     c                   z-add     1             codut
009700141201     c                   clear                   ds3a
009800141201     c                   eval      kcod='3A'
009900141201     c                   eval      kkey=blpcbo
010000141201     c     ktab          chain     tabel
010100141201     c                   if        %found(tabel00f )
010200141201     c                   movel     tbluni        ds3a
010300141201     c* escluse le recupero
010400141201     c                   endif
010500141201     c
010600151130     c* Verifico se è bolla mamma stampo
010700151130     c                   if        blpcbo<>'1'  and blpcbo<>'A'
010800151130     c                   except    rec
010900151130     c                   endif
011000151130     c
011100151130     c     kblp          chain     fnlbl02l
011200151130     c                   if        %found(fnlbl02l)
011300151130     c                   except    leg
011400151130     c                   endif
011500141201     c* aggiorno
011600141201     c                   exsr      Aggiorna
011700151130     c                   endif
011800141201     c
011900151130     c                   read      inticom_01
012000141201     c                   enddo
012100141201     c
012200141201     c
012300141201     c                   ENDSR
012400141201     O*------------------------------------------------------------------------*
012500141201     C     AGGIORNA      BEGSR
012600151130
012700151130     c                   if        blpcbo='1 '
012800151130     c                   eval      blpcbo='A '
012900151130     c*****              update    fnblp000
013000151130     c                   endif
013100151130
013200151130     c     kblp          chain     fnarb01l
013300151130    1c                   if        %found(fnarb01l)
013400151130     c                   if        arbcbo<>'1 ' and arbcbo<>'A '
013500151130     c                   except    reca
013600151130     c                   else
013700151130     c                   eval      arbcbo='A '
013800151130     c******             update    fnarb000
013900151130    1c                   endif
014000151130    1c                   endif
014100141210
014200141210     C                   SETOFF                                       020304
014300151130     c     kblp          setgt     titas30c
014400151130     c     kblp          readpe(e) titas30c
014500151130    1c                   if        %error
014600151130     c                   eval      contaerr+=1
014700151130   x1c                   else
014800151130    2c                   if        not %found(titas30c)
014900151130     c                   except    manca
015000151130   x2c                   else
015100151130     c                   if        tastbl<>'F1' and tastbl<>'F7'
015200151130     c                   except    rect
015300151130     c                   else
015400151130     c
015500151130     c                   eval      tastbl='F7'
015600151130     c***02              update    titas000
015700151130     c   02              eval      contagg+=1
015800151130     c***02              except    agg
015900151130    3c                   if        *in03 or *in04
016000151130     c                   except    fatt
016100151130    3c                   endif
016200151130    3c                   endif
016300151130    2c                   endif
016400151130    1c                   endif
016500141201     c
016600141201     c                   ENDSR
016700151130     Oqsysprt   e            tot         1
016800151130     o                                              'Bolle allocate          '
016900151130     o                       contaerr      z  +   1
017000151130     o                                        +   1 'Bolle aggiorante'
017100151130     o                       contagg       z  +   1
017200151130     o          e            notr        1
017300151130     o                                              'Bolla non trovata  BLP  '
017400151130     o                       lnp           z  +   1
017500151130     o                       nrs           z  +   1
017600151130     o                       nsp           z  +   1
017700151130     o          e            rec         1
017800151130     o                                              'Bolla NON 1 e A in BLP  '
017900151130     o                       lnp           z  +   1
018000151130     o                       nrs           z  +   1
018100151130     o                       nsp           z  +   1
018200151130     o          e            recA        1
018300151130     o                                              'Bolla NON 1 e A in ARB  '
018400151130     o                       lnp           z  +   1
018500151130     o                       nrs           z  +   1
018600151130     o                       nsp           z  +   1
018700151130     o          e            recT        1
018800151130     o                                              'Bolla NON 1 e A in TAS  '
018900151130     o                       lnp           z  +   1
019000151130     o                       nrs           z  +   1
019100151130     o                       nsp           z  +   1
019200151130     o          e            leg         1
019300151130     o                                              'Bolla mamma legata      '
019400151130     o                       lnp           z  +   1
019500151130     o                       nrs           z  +   1
019600151130     o                       nsp           z  +   1
019700151130     o                       blpcca           +   1
019800151130     o          e            manca       1
019900151130     o                                              'Bolla mancante in TITAS '
020000151130     o                       lnp           z  +   1
020100151130     o                       nrs           z  +   1
020200151130     o                       nsp           z  +   1
020300151130     o          e            fatt        1
020400151130     o                                              'Bolla fatturata in TITAS'
020500151130     o                       lnp           z  +   1
020600151130     o                       nrs           z  +   1
020700151130     o                       nsp           z  +   1
020800151130     o          e            agg         1
020900151130     o                                              'Bolla aggiornata TITAS  '
021000151130     o                       lnp           z  +   1
021100151130     o                       nrs           z  +   1
021200151130     o                       nsp           z  +   1
021300151130     o                       tascbo           +   1
021400151130     o                       tastbl           +   1
021500970910     O*------------------------------------------------------------------------*
