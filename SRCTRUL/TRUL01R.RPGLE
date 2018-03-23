000100120618     /*PRM  dbgview(*source)
000200130114     /*END
000300130114     **CMD  ovrdbf file(WFPRM01L) tofile(UNITRAGRP/WFPRM01L) +
000400130114     **CMD         ovrscope(*calllvl)
000500130114     **END  dltovr file(WFPRM01L) lvl(*)
000600130114     **END
000700070628      *---------------------------------------------------------------*
000800070628      *?Utility di REGISTRAZIONE o REPERIMENTO parametri nel WorkFile?*
000900070628      *?WFPRM00F per applicazioni BATCH                              ?*
001000070628      *---------------------------------------------------------------*
001100070628     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001200070628      *
001300070628      *?A R C H I V I?------------------------------------------------*
001400070628      *
001500070628     fWFPRM01L  Uf A e           k disk
001600070628      *
001700070628      *?C O S T A N T I?----------------------------------------------*
001800070628      *
001900070628      *
002000070628      *?S C H I E R E?------------------------------------------------*
002100070628      *
002200070628      *
002300070628      *?S T R U T T U R E   D A T I?----------------------------------*
002400070628      *
002500070628      * - Parametri
002600070628     d TRUL01ds      e ds
002700070628      *
002800070628     d Status         sds           333
002900070628     d  SDSpgm           *proc
003000070628     d  SDSprm           *parms
003100070628     d  SDSusr               254    263
003200070628      *
003300070628      *?V A R I A B I L I?--------------------------------------------*
003400070628      *
003500120612     d wDate_ISO       s               d   datfmt(*iso)  inz(*loval)
003600120612     d wDateLim        s                   like(wPRMdta) inz(*hival)
003700070628      *
003800070628      *?I N D I C A T O R I?------------------------------------------*
003900070628      *
004000070628      *
004100070628      *?K E Y - L I S T?----------------------------------------------*
004200070628      *
004300070628      *
004400070628      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
004500070628      *
004600070628     c     *Entry        plist
004700070628     c                   parm                    TRUL01ds
004800070628     c                   eval      UL01err  = *off
004900070628      *
005000070628      * Esecuzione opzione richiesta
005100070628      *
005200070628sel 1c                   select
005300070628      *
005400120612      * - Inserimento nuovi parametri (e cancellazione dei vecchi)
005500070628w   1c                   when      UL01opz  = *on
005600120612     c                   exsr      sr_DeleteOld
005700070628     c                   exsr      sr_Insert
005800071121      *
005900120612      * - Reperimento parametri (e cancellazione dei vecchi)
006000071121w   1c                   when      UL01opz  = *blanks
006100071121     c                   exsr      sr_Find
006200120612     c                   exsr      sr_DeleteOld
006300070628      *
006400070628      * - Reperimento e Cancellazione parametri
006500070628w   1c                   when      UL01opz  = *off
006600120612     c                   exsr      sr_DeleteOld
006700070628     c                   exsr      sr_Delete
006800120612      *
006900120612      * - Cancellazione vecchi parametri
007000120612w   1c                   when      UL01opz  = 'D'
007100120612     c                   exsr      sr_DeleteOld
007200070628      *
007300070628      * - Opzione errata
007400070628x   1c                   other
007500070628     c                   eval      UL01err  = *on
007600070628      *
007700070628e   1c                   endsl
007800070628      *
007900070628      * Fine
008000070628      *
008100070628     c                   eval      *inLR    = *on
008200070628      *
008300070628      *?S U B R O U T I N E S?----------------------------------------*
008400070628      *
008500070628      *---------------------------------------------------------------*
008600070628      *?Inserimento nuovi parametri                                  ?*
008700070628      *---------------------------------------------------------------*
008800070628     c     sr_Insert     BegSr
008900070628      *
009000070628      * Reperimento nuovo numero progressivo
009100070628     c     *hival        setgt     WFPRM000
009200070628     c                   readp(n)  WFPRM000
009300121128sel 1c                   select
009400121128w   1c                   when      %eof(WFPRM01L)
009500070628     c                   eval      UL01prg  = 1
009600121128w   1c                   when      wPRMprg  = *hival
009700121128     c                   eval      UL01prg  = 1
009800121128x   1c                   other
009900070628     c                   eval      UL01prg  = wPRMprg + 1
010000121128e   1c                   endsl
010100070628      *
010200070628      * Impostazione parametri nell'apposito campo
010300070628     c                   clear                   WFPRM000
010400070628     c                   eval      wPRMprg  = UL01prg
010500070628     c                   movel     *date         wDate_ISO
010600070628     c     *iso          movel     wDate_ISO     wPRMdta
010700070628     c                   eval      wPRMprf  = SDSusr
010800070628     c                   eval      wPRMazn  = UL01azn
010900070628     c                   eval      wPRMprm  = UL01prm
011000070628      *                  __________________
011100070628     c                   WRITE(e)  WFPRM000
011200070628      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
011300070628do  1c                   DOW       %error
011400121128if  2c                   if        wPRMprg  = *hival
011500121128     c                   eval      wPRMprg  = 1
011600121128x   2c                   else
011700070628     c                   eval      wPRMprg  = wPRMprg + 1
011800121128e   2c                   endif
011900070628     c                   eval      UL01prg  = wPRMprg
012000070628     c                   write(e)  WFPRM000
012100070628e   1c                   ENDDO
012200121128      *
012300121128     c                   eval      UL01prg  = wPRMprg
012400070628      *
012500070628     c                   EndSr
012600071121      *
012700071121      *---------------------------------------------------------------*
012800071121      *?Reperimento parametri                                        ?*
012900071121      *---------------------------------------------------------------*
013000071121     c     sr_Find       BegSr
013100071121      *
013200071121     c     UL01prg       chain(N)  WFPRM000
013300071121     c                   exsr      Out_DS
013400071121      *
013500071121     c                   EndSr
013600070628      *
013700070628      *---------------------------------------------------------------*
013800070628      *?Reperimento e Cancellazione parametri                        ?*
013900070628      *---------------------------------------------------------------*
014000070628     c     sr_Delete     BegSr
014100070628      *
014200070628     c     UL01prg       CHAIN     WFPRM000
014300071121     c                   exsr      Out_DS
014400070628      *
014500070628if  1c                   if        %found(WFPRM01L)
014600070628      *                  __________________
014700070628     c                   DELETE    WFPRM000
014800070628      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
014900070628e   1c                   endif
015000070628      *
015100070628     c                   EndSr
015200120612      /free
015300120612       //--------------------------------------------------------------
015400120612       //?Cancellazione vecchi parametri                               ?
015500120612       //--------------------------------------------------------------
015600120612       BEGSR  sr_DeleteOld;
015700120612
015800121128         // -?Reperimento data odierna - 3 mesi?
015900120618         wDateLim = %int( %subst( %char( %dec( %timestamp() - %months(3) ) )
016000120612                                  : 1 : 8 ) );
016100120612
016200120612         setll  (*loval)  WFPRM000;
016300120612         read   WFPRM000;
016400120612
016500120612         DoW  Not %eof(WFPRM01L);
016600120612
016700120612           if  wPRMdta <= wDateLim;
016800120612             //_______________
016900120612             DELETE  WFPRM000;
017000120612             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
017100120612           endif;
017200120612
017300120612           read  WFPRM000;
017400120612
017500120612         EndDo;
017600120612
017700120612       EndSr;
017800120612
017900120612      /end-free
018000071121      *---------------------------------------------------------------*
018100071121      *?Impostazione parametri di output                             ?*
018200071121      *---------------------------------------------------------------*
018300071121     c     Out_DS        BegSr
018400071121      *
018500071121if  1c                   if        %found(WFPRM01L)
018600071121      *
018700071121     c                   eval      UL01azn  = wPRMazn
018800071121     c                   eval      UL01prm  = wPRMprm
018900071121      *
019000071121x   1c                   else
019100071121      *
019200071121     c                   clear                   UL01azn
019300071121     c                   clear                   UL01prm
019400071121     c                   eval      UL01err  = *on
019500071121      *
019600071121e   1c                   endif
019700071121      *
019800071121     c                   EndSr
