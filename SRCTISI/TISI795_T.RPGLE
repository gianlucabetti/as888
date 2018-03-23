000100120223     ***************************************************************************
000200120223     **
000300150518     ** Tester di tisi795r
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700150518     Ftisi795_d CF   E             WORKSTN
000800120223
000900120223     ***************************************************************************
001000120223     **
001100120223     ** Costanti.
001200120223     **
001300120223     ***************************************************************************
001400120223
001500120223     ***************************************************************************
001600120223     **
001700120223     ** Strutture dati.
001800120223     **
001900120223     ***************************************************************************
002000150518     D tisi795i0     E DS                  INZ(*EXTDFT) QUALIFIED
002100150518     D tisi795o0     E DS                  INZ(*EXTDFT) QUALIFIED
002200150518       // -?Messaggi
002300150518     d tisi795m0     e ds                  QUALIFIED INZ(*EXTDFT)
002400150518     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
002500150518     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
002600150518     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
002700150518     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
002800150518       // -?Forzature
002900150518     d tisi795f0     e ds                  QUALIFIED INZ(*EXTDFT)
003000150518     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
003100150518     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
003200150518     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
003300150518       //--------------------------------------------------------------
003400150518       //?Definizione parametri programma.
003500150522     D prmRqsOpCode    s             10I 0
003600150522     D prmRpyOpCode    s             10I 0
003700150522     D prmRpyIdMsg     s             10I 0
003800150522     D prmRqsFormato   s             10A
003900150522     D prmRqsData      s          32767
004000150522     D prmRqsDataSize  s             10I 0
004100150522     D prmRpyFormato   s             10A
004200150522     D prmRpyData      s          32767
004300150522     D prmRpyDataSize  s             10I 0
004400150522     D prmRpyFormMsg   s             10A
004500150522     D prmRpyMessage   s          32767
004600150522     D prmRpyMsgSize   s             10I 0
004700150522     D prmRpyFormForz  s             10A
004800150522     D prmRpyForzatu   s          32767
004900150522     D prmRpyForSize   s             10I 0
005000150522
005100150629     d Dataiso         s               d   datfmt(*iso) inz(*sys)
005200150629     d Dataeur         s               d   datfmt(*eur)
005300150518       //--------------------------------------------------------------
005400150518       //?Definizione prototipi.
005500150518       //--------------------------------------------------------------
005600150518      /COPY GAITRASRC/SRCPROTOPR,Tisi795R
005700150518      /copy gaitrasrc/srcconst,TISI795R
005800120223     ***************************************************************************
005900120223     **
006000120223     ** Campi.
006100120223     **
006200120223     ***************************************************************************
006300120223
006400120223     ***************************************************************************
006500120223     **
006600120223     ** Main.
006700120223     **
006800120223     ***************************************************************************
006900120223
007000120223      /FREE
007100120224
007200120224       *INLR = *ON;
007300150522       prv_P='BO'   ;
007400150522       loc_P='BOLOGNA' ;
007500150522       CAP_P='40121  ' ;
007600150629       dataeur=dataiso  ;
007700150629       spediz=%char(dataeur) ;
007800150629       pesokg=10  ;
007900120224
008000120306 1     dow  not *inkc   ;
008100120306
008200150518       exfmt  si7950d  ;
008300120306
008400150629 2     if not *inkc   and  *inkf ;
008500120224
008600150518       RESET tisi795i0;
008700120306
008800150518       prmrqsformato='TISI795I0'  ;
008900150518       prmrpyformato='TISI795O0'  ;
009000150518       prmRpyFormMsg='TISI795M0'  ;
009100150518       tisi795i0.formato='TISI795I0' ;
009200150518       tisi795i0.lingua =lingua      ;
009300150518       tisi795i0.ksu    =ksu         ;
009400150529       tisi795i0.sun    =sun         ;
009500150518       tisi795i0.prv_p  =prv_p       ;
009600150518       tisi795i0.loc_p  =loc_p       ;
009700150518       tisi795i0.cap_p  =cap_p       ;
009800150518       tisi795i0.prv_a  =prv_a       ;
009900150518       tisi795i0.loc_a  =loc_a       ;
010000150518       tisi795i0.cap_a  =cap_a       ;
010100150529       tisi795i0.data_rit=spediz      ;
010200150518       tisi795i0.orm_comm=orm_comm    ;
010300150518       tisi795i0.merce_mag=merce_mag  ;
010400150518       tisi795i0.cons_fd  =cons_fd    ;
010500150518       tisi795i0.pesokg   =pesokg     ;
010600150518       tisi795i0.vol_mc   =vol_mc     ;
010700150518         prmrqsData = TISI795i0;
010800150518         prmrqsDatasize = %SIZE(TISI795i0);
010900150518         prmrpyDatasize = %SIZE(TISI795o0);
011000150518         prmRpyMsgSize  = %SIZE(TISI795m0);
011100150518         prmRpyForSize  = %SIZE(TISI795f0);
011200150518         prmRQSOPCODE   = TISI795_RQSOPCODE_GET_TEMPI2      ;
011300150518
011400150518         Tisi795_GetTempi2 (prmRqsOpCode : prmRpyOpCode : prmRpyIdMsg :
011500150518                            prmRqsFormato : prmRqsData : prmRqsDataSize:
011600150518                               prmRpyFormato : prmRpyData : prmRpyDataSize:
011700150518                               prmRpyFormMsg : prmRpyMessage : prmRpyMsgSize) ;
011800150518
011900150518         opcode= prmrpyopcode  ;
012000150518
012100150518 3       if   prmrpyOpCode = TISI795_RPYOPCODE_ERROR  ;
012200150518           outputmsg=%subst(prmrpymessage:1785:1040) ;
012300150518 x3      else ;
012400150518         TISI795O0 = prmrpyData;
012500150601         day_rit =tisi795o0.day_rit ;
012600150601         fil_fp  =tisi795o0.fil_fp  ;
012700150518         des_fp  =tisi795o0.des_fp  ;
012800150518         ind_fp  =tisi795o0.ind_fp  ;
012900150518         loc_fp  =tisi795o0.loc_fp  ;
013000150518         prv_fp  =tisi795o0.prv_fp  ;
013100150518         cap_fp  =tisi795o0.cap_fp  ;
013200150518         tel_fp  =tisi795o0.tel_fp  ;
013300150518         fax_fp  =tisi795o0.fax_fp  ;
013400150518         Mailpar =tisi795o0.mail_fp  ;
013500150629         Urlpar  =tisi795o0.url_fp  ;
013600150518         fil_fa  =tisi795o0.fil_fa  ;
013700150518         des_fa  =tisi795o0.des_fa  ;
013800150518         ind_fa  =tisi795o0.ind_fa  ;
013900150518         loc_fa  =tisi795o0.loc_fa  ;
014000150518         prv_fa  =tisi795o0.prv_fa  ;
014100150518         cap_fa  =tisi795o0.cap_fa  ;
014200150518         tel_fa  =tisi795o0.tel_fa  ;
014300150518         fax_fa  =tisi795o0.fax_fa  ;
014400150518         MailaRR =tisi795o0.mail_fa  ;
014500150629         Urlarr  =tisi795o0.url_fa  ;
014600150518         tsp_tot =tisi795o0.tsp_tot ;
014700150518         tsp_cod =tisi795o0.tsp_cod ;
014800150518         tsp_dispo=tisi795o0.tsp_dispo ;
014900150518         tsp_descr=tisi795o0.tsp_descr ;
015000150518         tsp_not  =tisi795o0.tsp_not   ;
015100150526         not1_rit =tisi795o0.not1_rit  ;
015200150526         not2_rit =tisi795o0.not2_rit  ;
015300150518
015400150518 3       endif  ;
015500120306 2         endif  ;
015600120306
015700120306 1        enddo  ;
015800120224
015900120224      /END-FREE
