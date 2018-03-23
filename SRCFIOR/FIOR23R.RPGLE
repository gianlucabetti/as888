000100040921     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200040817      *-------------------------------------------------------------------------*
000300040817      * Pulizia Anagrafica ritiri e anagrafica ritiri fissi
000400040817      *-------------------------------------------------------------------------*
000500130926     ffnacr00f  uf   e             disk    infds(fnacr_infds)
000600130927     ffnacr11l  uf   e           k disk
000700151013     ffnacre1l  uf   e           k disk
000800130926     ffnors00f  uf   e             disk    infds(fnors_infds)
000900130927     ffnors11l  uf   e           k disk
001000040921     fTabel00f  if   e           k disk
001100040921
001200040921     d codut           s                   like(tblkut) inz(1)
001300040921     d kcod            s                   like(tblcod) inz('5A')
001400040921     d kkey            s                   like(tblkey) inz('SEDE1')
001500040921
001600040921     d datamax         s               d   datfmt(*iso)
001700040921     d dataoggi        s               d   datfmt(*iso)
001800040921     d wdata           s              8  0
001900040921
002000040921     d ds5as1        e ds
002100040817     d kpjba         e ds
002200130926
002300130926     d fnacr_infds     ds
002400130926     d   ACRrrn              397    400i 0
002500130926     d fnors_infds     ds
002600130926     d   ORSrrn              397    400i 0
002700130927
002800130927     c     k02fnors11    klist
002900130927     c                   kfld                    ORSpoe
003000130927     c                   kfld                    ORSnor
003100040817
003200040817      *-------------------------------------------------------------------------*
003300040817
003400040817      * Anagrafica ritiri deleto i record annullati
003500040817     c                   Do        *Hival
003600130926     c                   Read(N)   Fnacr00f
003700040817     c                   If        %Eof(Fnacr00f)
003800040817     c                   Leave
003900040817     c                   EndIf
004000040817
004100040817     c                   If        AcrAtb = *blanks
004200040817     c                   Iter
004300040817     c                   EndIf
004400040921
004500040921     c                   If        AcrDtr >= wdata
004600040921     c                   Iter
004700040921     c                   EndIf
004800040817
004900130926     c     ACRcro        chain(E)  FNACR100
005000071022     c                   dow       %found(FNACR11L)
005100130926     c                             and  Not  %error
005200130926     c     ACRcro        DELETE(e) FNACR100
005300071022     c                   enddo
005400130926     c                   if        %error
005500130926     c                   iter
005600130926     c                   endif
005700151013
005800151013     c     ACRcro        chain(E)  FNACRE00
005900151013     c                   dow       %found(FNACRE1L)
006000151013     c                             and  Not  %error
006100151013     c     ACRcro        DELETE(e) FNACRE00
006200151013     c                   enddo
006300151013     c                   if        %error
006400151013     c                   iter
006500151013     c                   endif
006600070803      *
006700130926     c     ACRrrn        Delete(E) Fnacr000
006800130926     c                   if        %error
006900130926     c                   iter
007000130926     c                   endif
007100040817
007200040817     c                   EndDo
007300040817
007400040817      * Anagrafica ritiri fissi deleto i record annullati
007500040817     c                   Do        *Hival
007600130926     c                   Read(N)   Fnors00f
007700040817     c                   If        %Eof(Fnors00f)
007800040817     c                   Leave
007900040817     c                   EndIf
008000040817
008100040817     c                   If        OrsAtb = *blanks
008200040817     c                   Iter
008300040817     c                   EndIf
008400040921
008500040921     c                   If        OrsDtt >= wdata
008600040921     c                   Iter
008700040921     c                   EndIf
008800130927
008900130927     c     k02fnors11    chain(E)  FNORS100
009000130927     c                   dow       %found(FNORS11L)
009100130927     c                             and  Not  %error
009200130927     c     k02fnors11    DELETE(e) FNORS100
009300130927     c                   enddo
009400130927     c                   if        %error
009500130927     c                   iter
009600130927     c                   endif
009700040817
009800130926     c     ORSrrn        Delete(E) Fnors000
009900130926     c                   if        %error
010000130926     c                   iter
010100130926     c                   endif
010200040817
010300040817     c                   EndDo
010400040817
010500040817     c                   Eval      *InLr = *On
010600040817
010700040817      *-------------------------------------------------------------------------*
010800040817      * Subroutine iniziale
010900040817      *-------------------------------------------------------------------------*
011000040817     c     *Inzsr        BegSr
011100040817
011200040817     c     *Entry        Plist
011300040817     c                   Parm                    kpjba
011400040921
011500040921     c     ktabel        klist
011600040921     c                   kfld                    codut
011700040921     c                   kfld                    kcod
011800040921     c                   kfld                    kkey
011900040921
012000040921     c                   Clear                   ds5as1
012100040921     c     ktabel        Chain     Tabel00f
012200040921     c                   If        %Found(Tabel00f)
012300040921     c                   Eval      ds5as1 = Tbluni
012400040921     c                   EndIf
012500040921
012600040921     c                   Move      *date         dataoggi
012700040921     c     dataoggi      subdur    §5aacr:*d     datamax
012800040921     c                   move      datamax       wdata
012900040817
013000040817     c                   EndSr
