000100120618     /*PRM  dbgview(*source)
000200120618     /*CMD  ovrdbf file(WFPRM01L) tofile(GAITRAAZP/WFPRM01L) +
000300120618     /*CMD         ovrscope(*calllvl)
000400120618     /*END  dltovr file(WFPRM01L) lvl(*)
000500120618     /*END
000600070628      *---------------------------------------------------------------*
000700070628      *?Utility di REGISTRAZIONE o REPERIMENTO parametri nel WorkFile?*
000800070628      *?WFPRM00F per applicazioni BATCH                              ?*
000900070628      *---------------------------------------------------------------*
001000070628     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100070628      *
001200070628      *?A R C H I V I?------------------------------------------------*
001300070628      *
001400070628     fWFPRM01L  Uf A e           k disk
001500070628      *
001600070628      *?C O S T A N T I?----------------------------------------------*
001700070628      *
001800070628      *
001900070628      *?S C H I E R E?------------------------------------------------*
002000070628      *
002100070628      *
002200070628      *?S T R U T T U R E   D A T I?----------------------------------*
002300070628      *
002400070628      * - Parametri
002500070628     d TRUL01ds      e ds
002600070628      *
002700070628     d Status         sds           333
002800070628     d  SDSpgm           *proc
002900070628     d  SDSprm           *parms
003000070628     d  SDSusr               254    263
003100070628      *
003200070628      *?V A R I A B I L I?--------------------------------------------*
003300070628      *
003400120612     d wDate_ISO       s               d   datfmt(*iso)  inz(*loval)
003500120612     d wDateLim        s                   like(wPRMdta) inz(*hival)
003600070628      *
003700070628      *?I N D I C A T O R I?------------------------------------------*
003800070628      *
003900070628      *
004000070628      *?K E Y - L I S T?----------------------------------------------*
004100070628      *
004200070628      *
004300070628      *?C I C L O   D I   E L A B O R A Z I O N E?--------------------*
004400070628      *
004500070628     c     *Entry        plist
004600070628     c                   parm                    TRUL01ds
004700070628     c                   eval      UL01err  = *off
004800070628      *
004900070628      * Esecuzione opzione richiesta
005000070628      *
005100070628sel 1c                   select
005200070628      *
005300120612      * - Inserimento nuovi parametri (e cancellazione dei vecchi)
005400070628w   1c                   when      UL01opz  = *on
005500120612     c                   exsr      sr_DeleteOld
005600070628     c                   exsr      sr_Insert
005700071121      *
005800120612      * - Reperimento parametri (e cancellazione dei vecchi)
005900071121w   1c                   when      UL01opz  = *blanks
006000071121     c                   exsr      sr_Find
006100120612     c                   exsr      sr_DeleteOld
006200070628      *
006300070628      * - Reperimento e Cancellazione parametri
006400070628w   1c                   when      UL01opz  = *off
006500120612     c                   exsr      sr_DeleteOld
006600070628     c                   exsr      sr_Delete
006700120612      *
006800120612      * - Cancellazione vecchi parametri
006900120612w   1c                   when      UL01opz  = 'D'
007000120612     c                   exsr      sr_DeleteOld
007100070628      *
007200070628      * - Opzione errata
007300070628x   1c                   other
007400070628     c                   eval      UL01err  = *on
007500070628      *
007600070628e   1c                   endsl
007700070628      *
007800070628      * Fine
007900070628      *
008000070628     c                   eval      *inLR    = *on
008100070628      *
008200070628      *?S U B R O U T I N E S?----------------------------------------*
008300070628      *
008400070628      *---------------------------------------------------------------*
008500070628      *?Inserimento nuovi parametri                                  ?*
008600070628      *---------------------------------------------------------------*
008700070628     c     sr_Insert     BegSr
008800070628      *
008900070628      * Reperimento nuovo numero progressivo
009000070628     c     *hival        setgt     WFPRM000
009100070628     c                   readp(n)  WFPRM000
009200121128sel 1c                   select
009300121128w   1c                   when      %eof(WFPRM01L)
009400070628     c                   eval      UL01prg  = 1
009500121128w   1c                   when      wPRMprg  = *hival
009600121128     c                   eval      UL01prg  = 1
009700121128x   1c                   other
009800070628     c                   eval      UL01prg  = wPRMprg + 1
009900121128e   1c                   endsl
010000070628      *
010100070628      * Impostazione parametri nell'apposito campo
010200070628     c                   clear                   WFPRM000
010300070628     c                   eval      wPRMprg  = UL01prg
010400070628     c                   movel     *date         wDate_ISO
010500070628     c     *iso          movel     wDate_ISO     wPRMdta
010600070628     c                   eval      wPRMprf  = SDSusr
010700070628     c                   eval      wPRMazn  = UL01azn
010800070628     c                   eval      wPRMprm  = UL01prm
010900070628      *                  __________________
011000070628     c                   WRITE(e)  WFPRM000
011100070628      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
011200070628do  1c                   DOW       %error
011300121128if  2c                   if        wPRMprg  = *hival
011400121128     c                   eval      wPRMprg  = 1
011500121128x   2c                   else
011600070628     c                   eval      wPRMprg  = wPRMprg + 1
011700121128e   2c                   endif
011800070628     c                   eval      UL01prg  = wPRMprg
011900070628     c                   write(e)  WFPRM000
012000070628e   1c                   ENDDO
012100121128      *
012200121128     c                   eval      UL01prg  = wPRMprg
012300070628      *
012400070628     c                   EndSr
012500071121      *
012600071121      *---------------------------------------------------------------*
012700071121      *?Reperimento parametri                                        ?*
012800071121      *---------------------------------------------------------------*
012900071121     c     sr_Find       BegSr
013000071121      *
013100071121     c     UL01prg       chain(N)  WFPRM000
013200071121     c                   exsr      Out_DS
013300071121      *
013400071121     c                   EndSr
013500070628      *
013600070628      *---------------------------------------------------------------*
013700070628      *?Reperimento e Cancellazione parametri                        ?*
013800070628      *---------------------------------------------------------------*
013900070628     c     sr_Delete     BegSr
014000070628      *
014100070628     c     UL01prg       CHAIN     WFPRM000
014200071121     c                   exsr      Out_DS
014300070628      *
014400070628if  1c                   if        %found(WFPRM01L)
014500070628      *                  __________________
014600070628     c                   DELETE    WFPRM000
014700070628      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
014800070628e   1c                   endif
014900070628      *
015000070628     c                   EndSr
015100120612      /free
015200120612       //--------------------------------------------------------------
015300120612       //?Cancellazione vecchi parametri                               ?
015400120612       //--------------------------------------------------------------
015500120612       BEGSR  sr_DeleteOld;
015600120612
015700121128         // -?Reperimento data odierna - 3 mesi?
015800120618         wDateLim = %int( %subst( %char( %dec( %timestamp() - %months(3) ) )
015900120612                                  : 1 : 8 ) );
016000120612
016100120612         setll  (*loval)  WFPRM000;
016200120612         read   WFPRM000;
016300120612
016400120612         DoW  Not %eof(WFPRM01L);
016500120612
016600120612           if  wPRMdta <= wDateLim;
016700120612             //_______________
016800120612             DELETE  WFPRM000;
016900120612             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
017000120612           endif;
017100120612
017200120612           read  WFPRM000;
017300120612
017400120612         EndDo;
017500120612
017600120612       EndSr;
017700120612
017800120612      /end-free
017900071121      *---------------------------------------------------------------*
018000071121      *?Impostazione parametri di output                             ?*
018100071121      *---------------------------------------------------------------*
018200071121     c     Out_DS        BegSr
018300071121      *
018400071121if  1c                   if        %found(WFPRM01L)
018500071121      *
018600071121     c                   eval      UL01azn  = wPRMazn
018700071121     c                   eval      UL01prm  = wPRMprm
018800071121      *
018900071121x   1c                   else
019000071121      *
019100071121     c                   clear                   UL01azn
019200071121     c                   clear                   UL01prm
019300071121     c                   eval      UL01err  = *on
019400071121      *
019500071121e   1c                   endif
019600071121      *
019700071121     c                   EndSr
