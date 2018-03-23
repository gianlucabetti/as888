000100040921     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200040817      *-------------------------------------------------------------------------*
000300040817      * Pulizia Anagrafica ritiri e anagrafica ritiri fissi
000400040817      *-------------------------------------------------------------------------*
000500130926     ffnacr00f  uf   e             disk    infds(fnacr_infds)
000600130927     ffnacr11l  uf   e           k disk
000700130926     ffnors00f  uf   e             disk    infds(fnors_infds)
000800130927     ffnors11l  uf   e           k disk
000900040921     fTabel00f  if   e           k disk
001000040921
001100040921     d codut           s                   like(tblkut) inz(1)
001200040921     d kcod            s                   like(tblcod) inz('5A')
001300040921     d kkey            s                   like(tblkey) inz('SEDE1')
001400040921
001500040921     d datamax         s               d   datfmt(*iso)
001600040921     d dataoggi        s               d   datfmt(*iso)
001700040921     d wdata           s              8  0
001800040921
001900040921     d ds5as1        e ds
002000040817     d kpjba         e ds
002100130926
002200130926     d fnacr_infds     ds
002300130926     d   ACRrrn              397    400i 0
002400130926     d fnors_infds     ds
002500130926     d   ORSrrn              397    400i 0
002600130927
002700130927     c     k02fnors11    klist
002800130927     c                   kfld                    ORSpoe
002900130927     c                   kfld                    ORSnor
003000040817
003100040817      *-------------------------------------------------------------------------*
003200040817
003300040817      * Anagrafica ritiri deleto i record annullati
003400040817     c                   Do        *Hival
003500130926     c                   Read(N)   Fnacr00f
003600040817     c                   If        %Eof(Fnacr00f)
003700040817     c                   Leave
003800040817     c                   EndIf
003900040817
004000040817     c                   If        AcrAtb = *blanks
004100040817     c                   Iter
004200040817     c                   EndIf
004300040921
004400040921     c                   If        AcrDtr >= wdata
004500040921     c                   Iter
004600040921     c                   EndIf
004700040817
004800130926     c     ACRcro        chain(E)  FNACR100
004900071022     c                   dow       %found(FNACR11L)
005000130926     c                             and  Not  %error
005100130926     c     ACRcro        DELETE(e) FNACR100
005200071022     c                   enddo
005300130926     c                   if        %error
005400130926     c                   iter
005500130926     c                   endif
005600070803      *
005700130926     c     ACRrrn        Delete(E) Fnacr000
005800130926     c                   if        %error
005900130926     c                   iter
006000130926     c                   endif
006100040817
006200040817     c                   EndDo
006300040817
006400040817      * Anagrafica ritiri fissi deleto i record annullati
006500040817     c                   Do        *Hival
006600130926     c                   Read(N)   Fnors00f
006700040817     c                   If        %Eof(Fnors00f)
006800040817     c                   Leave
006900040817     c                   EndIf
007000040817
007100040817     c                   If        OrsAtb = *blanks
007200040817     c                   Iter
007300040817     c                   EndIf
007400040921
007500040921     c                   If        OrsDtt >= wdata
007600040921     c                   Iter
007700040921     c                   EndIf
007800130927
007900130927     c     k02fnors11    chain(E)  FNORS100
008000130927     c                   dow       %found(FNORS11L)
008100130927     c                             and  Not  %error
008200130927     c     k02fnors11    DELETE(e) FNORS100
008300130927     c                   enddo
008400130927     c                   if        %error
008500130927     c                   iter
008600130927     c                   endif
008700040817
008800130926     c     ORSrrn        Delete(E) Fnors000
008900130926     c                   if        %error
009000130926     c                   iter
009100130926     c                   endif
009200040817
009300040817     c                   EndDo
009400040817
009500040817     c                   Eval      *InLr = *On
009600040817
009700040817      *-------------------------------------------------------------------------*
009800040817      * Subroutine iniziale
009900040817      *-------------------------------------------------------------------------*
010000040817     c     *Inzsr        BegSr
010100040817
010200040817     c     *Entry        Plist
010300040817     c                   Parm                    kpjba
010400040921
010500040921     c     ktabel        klist
010600040921     c                   kfld                    codut
010700040921     c                   kfld                    kcod
010800040921     c                   kfld                    kkey
010900040921
011000040921     c                   Clear                   ds5as1
011100040921     c     ktabel        Chain     Tabel00f
011200040921     c                   If        %Found(Tabel00f)
011300040921     c                   Eval      ds5as1 = Tbluni
011400040921     c                   EndIf
011500040921
011600040921     c                   Move      *date         dataoggi
011700040921     c     dataoggi      subdur    §5aacr:*d     datamax
011800040921     c                   move      datamax       wdata
011900040817
012000040817     c                   EndSr
