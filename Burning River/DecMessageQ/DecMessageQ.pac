| package |
package := Package name: 'DecMessageQ'.
package paxVersion: 0;
	basicComment: 'An interface to the DecMessageQ (now BEA MessageQ) product.  To use do something like the following:

	msg := DMQMessage new.
	msg
		bytes: ''A test message'';
		messageClass: 100;		"Phony value"
		type: 50.			"Phony value"

	tq := DMQTemporaryQueue new.
	tq attach.
	tq send: msg to: ''TEST_QUEUE''.
	tq detach.

Note: this code is unstable and has crashed images before.  USE AT YOUR OWN RISK!!!!!!!!!!

Copyright (c) 2002-2004 Robert Jarvis

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'.

package basicScriptAt: #preuninstall put: 'DMQStatus clear.
DMQConstants uninstall.'.

package classNames
	add: #DMQConstants;
	add: #DMQLibrary;
	add: #DMQMessage;
	add: #DMQPermanentQueue;
	add: #DMQQueue;
	add: #DMQStatus;
	add: #DMQTemporaryQueue;
	add: #PSB;
	add: #QAddress;
	add: #SelectionArrayComponent;
	add: #ShowBuffer;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #DMQConstants
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #DMQMessage
	instanceVariableNames: 'bytes sourceQAddress messageClass type sequenceNumber recoverable priority'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #DMQQueue
	instanceVariableNames: 'qAddress status defaultMessageBufferSize selectionFilter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #DMQStatus
	instanceVariableNames: 'code severity text'
	classVariableNames: 'InstanceDictionary'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DMQQueue subclass: #DMQPermanentQueue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DMQQueue subclass: #DMQTemporaryQueue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #DMQLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #PSB
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #QAddress
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #SelectionArrayComponent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #ShowBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DMQConstants guid: (GUID fromString: '{78315207-125E-11D5-BE08-00010240D5E2}')!
DMQConstants comment: ''!
!DMQConstants categoriesForClass!Unclassified! !
!DMQConstants methodsFor!

MSG_CLAS_ETHERNET
	^100!

MSG_CLAS_MRS
	^28!

MSG_CLAS_PAMS
	^29!

MSG_CLAS_TEST_CONTROL
	^1001!

MSG_CLAS_TEST_DATA
	^1000!

MSG_CLAS_UCB
	^102!

MSG_TYPE_ALLOCATED_SQ
	^-982!

MSG_TYPE_AVAIL
	^-1183!

MSG_TYPE_AVAIL_DEREG
	^-1181!

MSG_TYPE_AVAIL_REG
	^-1180!

MSG_TYPE_AVAIL_REG_REPLY
	^-1182!

MSG_TYPE_COM_SERVER_NAK
	^-989!

MSG_TYPE_DECLARE_SQ
	^-980!

MSG_TYPE_DEL_STATUS
	^-983!

MSG_TYPE_DEMO_ADD
	^-1200!

MSG_TYPE_DEMO_CHG
	^-1216!

MSG_TYPE_DEMO_CLIENT_DOWN
	^-1283!

MSG_TYPE_DEMO_CLIENT_UP
	^-1282!

MSG_TYPE_DEMO_DEL
	^-1232!

MSG_TYPE_DEMO_INQ
	^-1248!

MSG_TYPE_DEMO_LST
	^-1280!

MSG_TYPE_DEMO_LST_NEXT
	^-1281!

MSG_TYPE_DISABLE_NOTIFY
	^-991!

MSG_TYPE_DISABLE_QUOTAS
	^-2106!

MSG_TYPE_DISABLE_STATISTICS
	^-2108!

MSG_TYPE_E_CONNECT
	^-1000!

MSG_TYPE_E_CONNECT_COMPLETE
	^-1003!

MSG_TYPE_E_CONNECT_REJECTED
	^-1004!

MSG_TYPE_E_DISCONNECT
	^-1001!

MSG_TYPE_E_INITIALIZE
	^-1002!

MSG_TYPE_E_IO_ERROR
	^-1006!

MSG_TYPE_E_LISTEN_TIMEOUT
	^-1008!

MSG_TYPE_E_MESSAGES_LOST
	^-1009!

MSG_TYPE_E_PARTNER_DISC
	^-1005!

MSG_TYPE_E_PROTOCOL_ERROR
	^-1007!

MSG_TYPE_E_RUNTIME_ERROR
	^-1010!

MSG_TYPE_ENABLE_NOTIFY
	^-990!

MSG_TYPE_ENABLE_QUOTAS
	^-2105!

MSG_TYPE_ENABLE_STATISTICS
	^-2107!

MSG_TYPE_GCP_SHUTDOWN
	^-2102!

MSG_TYPE_GET_PROC_INFO
	^-984!

MSG_TYPE_GET_PROC_INFO_REP
	^-985!

MSG_TYPE_GROUP_DETAIL_REQ
	^-2000!

MSG_TYPE_GROUP_DETAIL_RESP
	^-2001!

MSG_TYPE_LINK_COMPLETE
	^-999!

MSG_TYPE_LINK_DISABLE_NOTIFY
	^-987!

MSG_TYPE_LINK_ENABLE_NOTIFY
	^-986!

MSG_TYPE_LINK_LOSS
	^self MSG_TYPE_LINK_LOST!

MSG_TYPE_LINK_LOST
	^-998!

MSG_TYPE_LIST_ALL_CONNECTIONS
	^-996!

MSG_TYPE_LIST_ALL_ENTRIES
	^-995!

MSG_TYPE_LIST_ALL_ENTRYS
	^self MSG_TYPE_LIST_ALL_ENTRIES!

MSG_TYPE_LIST_ALL_GROUPS
	^-997!

MSG_TYPE_LIST_DCLS
	^-994!

MSG_TYPE_MONITOR_DISPLAY_CTR
	^-915!

MSG_TYPE_MONITOR_DSPLY_QUOTA
	^-923!

MSG_TYPE_MONITOR_GRP_TBL_A
	^-952!

MSG_TYPE_MONITOR_GRP_TBL_G
	^-951!

MSG_TYPE_MONITOR_GRP_TBL_GT
	^-950!

MSG_TYPE_MONITOR_PROCESS
	^-905!

MSG_TYPE_MONITOR_RECONNECT
	^-953!

MSG_TYPE_MONITOR_RESET
	^-906!

MSG_TYPE_MONITOR_TERMINATE
	^-921!

MSG_TYPE_MONITORSTOP_ERLOG
	^-911!

MSG_TYPE_MOT_DEREG
	^-1154!

MSG_TYPE_MRS_ACK
	^-801!

MSG_TYPE_MRS_CNF
	^-840!

MSG_TYPE_MRS_CNF_ACK
	^-841!

MSG_TYPE_MRS_CNF_NAK
	^-842!

MSG_TYPE_MRS_DEBUG_OFF
	^-851!

MSG_TYPE_MRS_DEBUG_ON
	^-850!

MSG_TYPE_MRS_DQF_TRANSFER
	^-700!

MSG_TYPE_MRS_DQF_TRANSFER_ACK
	^-701!

MSG_TYPE_MRS_DQF_TRANSFER_REP
	^-702!

MSG_TYPE_MRS_SET_DLJ
	^-882!

MSG_TYPE_MRS_SET_DLJ_REP
	^-883!

MSG_TYPE_MRS_SET_PCJ
	^-880!

MSG_TYPE_MRS_SET_PCJ_REP
	^-881!

MSG_TYPE_MSG_STATUS
	^-983!

MSG_TYPE_NAME_ADD
	^-1190!

MSG_TYPE_NAME_ADD_REP
	^-1191!

MSG_TYPE_NAME_DEL
	^-1192!

MSG_TYPE_NAME_DEL_REP
	^-1193!

MSG_TYPE_NAME_DUMP
	^-1198!

MSG_TYPE_NAME_INQ_GBL_ID
	^-1199!

MSG_TYPE_NAME_INQ_ID
	^-1196!

MSG_TYPE_NAME_INQ_ID_REP
	^-1197!

MSG_TYPE_NAME_INQ_LCL_ID
	^-1196!

MSG_TYPE_NAME_INQ_NAME
	^-1194!

MSG_TYPE_NAME_INQ_NAME_REP
	^-1195!

MSG_TYPE_PROCESS_DCL
	^-992!

MSG_TYPE_PROCESS_EXIT
	^-993!

MSG_TYPE_Q_CLEANUP
	^-2100!

MSG_TYPE_Q_DETAIL_REQ
	^-2004!

MSG_TYPE_Q_DETAIL_RESP
	^-2005!

MSG_TYPE_Q_STATS_RESET
	^-2010!

MSG_TYPE_Q_SUMMARY_REQ
	^-2002!

MSG_TYPE_Q_SUMMARY_RESP
	^-2003!

MSG_TYPE_QUEUE_BYTE_QUOTAS
	^-2110!

MSG_TYPE_QUEUE_MESSAGE_QUOTAS
	^-2109!

MSG_TYPE_ROUTING_SUMMARY_REQ
	^-2012!

MSG_TYPE_ROUTING_SUMMARY_RESP
	^-2013!

MSG_TYPE_SBS_BS_SEQGAP
	^-1166!

MSG_TYPE_SBS_DEREG
	^-1174!

MSG_TYPE_SBS_DEREG_ACK
	^-1155!

MSG_TYPE_SBS_DUMP_TABLES
	^-1162!

MSG_TYPE_SBS_PURGE
	^-1157!

MSG_TYPE_SBS_PURGE_ACK
	^-1158!

MSG_TYPE_SBS_REG
	^-1150!

MSG_TYPE_SBS_REG_EZ
	^-1173!

MSG_TYPE_SBS_REG_EZ_REPLY
	^-1153!

MSG_TYPE_SBS_REG_REPLY
	^-1152!

MSG_TYPE_TEST_ACK
	^1030!

MSG_TYPE_TEST_DATA
	^1040!

MSG_TYPE_TEST_REQ
	^1020!

MSG_TYPE_TEST_SIGNOFF
	^1060!

MSG_TYPE_TEST_SIGNON
	^1010!

MSG_TYPE_TEST_STATUS
	^1050!

MSG_TYPE_TIMER_ADD
	^-960!

MSG_TYPE_TIMER_CANCEL
	^-961!

MSG_TYPE_TIMER_EXPIRED
	^-900!

MSG_TYPE_UCB_CONNECT
	^-1200!

MSG_TYPE_UCB_CONNECT_COMPLETE
	^-1203!

MSG_TYPE_UCB_DISCONNECT
	^-1201!

MSG_TYPE_UCB_INITIALIZE
	^-1202!

MSG_TYPE_UCB_IO_ERROR
	^-1206!

MSG_TYPE_UCB_MESSAGES_LOST
	^-1205!

MSG_TYPE_UCB_PARTNER_DISC
	^-1204!

MSG_TYPE_UCB_RCV_DATA
	^-1207!

MSG_TYPE_UNAVAIL
	^-1184!

MSG_TYPE_UNDECLARE_SQ
	^-981!

MSG_TYPE_XGROUP_CLEANUP
	^-2101!

MSG_TYPE_XGROUP_DETAIL_REQ
	^-2008!

MSG_TYPE_XGROUP_DETAIL_RESP
	^-2009!

MSG_TYPE_XGROUP_START
	^-2103!

MSG_TYPE_XGROUP_STATS_RESET
	^-2011!

MSG_TYPE_XGROUP_STOP
	^-2104!

MSG_TYPE_XGROUP_SUMMARY_REQ
	^-2006!

MSG_TYPE_XGROUP_SUMMARY_RESP
	^-2007!

PAMS__ABORT
	^-82!

PAMS__ACKTMO
	^-16!

PAMS__AREATOSMALL
	^-124!

PAMS__BADARGLIST
	^-226!

PAMS__BADASSIGN
	^-84!

PAMS__BADASTPARM
	^-140!

PAMS__BADCMD
	^-258!

PAMS__BADDECLARE
	^-10!

PAMS__BADDELIVERY
	^-60!

PAMS__BADFREE
	^-12!

PAMS__BADINITFILE
	^-282!

PAMS__BADJOURNAL
	^-178!

PAMS__BADLOGIC
	^-238!

PAMS__BADMSGBUF
	^-308!

PAMS__BADNAME
	^-254!

PAMS__BADPARAM
	^-186!

PAMS__BADPRIORITY
	^-58!

PAMS__BADPROCNUM
	^-62!

PAMS__BADQTYPE
	^-288!

PAMS__BADRECEIVE
	^-26!

PAMS__BADRESPQ
	^-224!

PAMS__BADSCRIPT
	^-102!

PAMS__BADSELIDX
	^-182!

PAMS__BADSEQ
	^-208!

PAMS__BADSYNCHNUM
	^-66!

PAMS__BADTAG
	^-248!

PAMS__BADTBQHANDLE
	^-250!

PAMS__BADTIME
	^-110!

PAMS__BADTMPPROC
	^-64!

PAMS__BADTMPSYNCH
	^-68!

PAMS__BADUMA
	^-222!

PAMS__BIGBLKSIZE
	^-24!

PAMS__BIGMSG
	^-120!

PAMS__BUFFEROVF
	^-174!

PAMS__BUSNOTSET
	^-290!

PAMS__CANCEL
	^-168!

PAMS__CIRACT
	^-78!

PAMS__COMMERR
	^-180!

PAMS__CONFIRMREQ
	^37!

PAMS__CONFLICT
	^-298!

PAMS__CREATEFAIL
	^-134!

PAMS__DCLTMPFAIL
	^-136!

PAMS__DECLARED
	^-106!

PAMS__DECNETDEAD
	^-104!

PAMS__DETACHED
	^41!

PAMS__DISC_FAILED
	^-190!

PAMS__DISC_SUCCESS
	^11!

PAMS__DISCL_FAILED
	^-192!

PAMS__DISCL_SUCCESS
	^13!

PAMS__DLJ_FAILED
	^-194!

PAMS__DLJ_SUCCESS
	^15!

PAMS__DLQ_FAILED
	^-196!

PAMS__DLQ_SUCCESS
	^17!

PAMS__DNSCLASSBAD
	^-310!

PAMS__DNSDIRFAIL
	^-312!

PAMS__DNSFMTBAD
	^-314!

PAMS__DQF_DEVICE_FAIL
	^-198!

PAMS__DQF_FULL
	^-202!

PAMS__DUPLQNAME
	^-244!

PAMS__ENQUEUED
	^33!

PAMS__EX_Q_LEN
	^-156!

PAMS__EXCEEDQUOTA
	^-56!

PAMS__EXCMAXUNCONF
	^-170!

PAMS__EXHAUSTBLKS
	^-108!

PAMS__EXPIRED
	^-296!

PAMS__FAILED
	^-320!

PAMS__FATAL
	^-272!

PAMS__GROUPNOTSET
	^-292!

PAMS__IDXTBLFULL
	^-184!

PAMS__INSQUEFAIL
	^-132!

PAMS__INTERNAL
	^-262!

PAMS__INVACCESS
	^-252!

PAMS__INVALIDID
	^-36!

PAMS__INVALIDNUM
	^-52!

PAMS__INVBUFFPTR
	^-176!

PAMS__INVFORMAT
	^-50!

PAMS__INVJH
	^-204!

PAMS__INVUCBCNTRL
	^-74!

PAMS__INVUMA
	^-200!

PAMS__JOURNAL_FAIL
	^-286!

PAMS__JOURNAL_FULL
	^-284!

PAMS__JOURNAL_ON
	^5!

PAMS__LINK_DOWN
	^-206!

PAMS__LINK_UP
	^3!

PAMS__LOGNAME
	^-90!

PAMS__LOGNAME2
	^-92!

PAMS__LOGNAME3
	^-94!

PAMS__LOGNAME4
	^-96!

PAMS__LOGNAME5
	^-98!

PAMS__MRQTBLFULL
	^-164!

PAMS__MRS_RES_EXH
	^-212!

PAMS__MSGACT
	^-8!

PAMS__MSGTOBIG
	^-22!

PAMS__MSGTOSMALL
	^-122!

PAMS__MSGUNDEL
	^-18!

PAMS__NAMETOOLONG
	^-256!

PAMS__NETERROR
	^-276!

PAMS__NETLINKLOST
	^-280!

PAMS__NETNOLINK
	^-278!

PAMS__NO_DQF
	^-228!

PAMS__NO_SAF
	^-230!

PAMS__NO_UMA
	^25!

PAMS__NOACCESS
	^-302!

PAMS__NOACL
	^-300!

PAMS__NOCANSEND
	^-126!

PAMS__NOLINK
	^-76!

PAMS__NOMOREJH
	^-214!

PAMS__NOMOREMSG
	^7!

PAMS__NOMRQRESRC
	^-242!

PAMS__NOMRS
	^-188!

PAMS__NOOBJECT
	^-166!

PAMS__NOOPEN
	^-100!

PAMS__NOPRIV
	^-316!

PAMS__NOQUOTA
	^-264!

PAMS__NOSEND
	^-2!

PAMS__NOSUCHPCJ
	^-218!

PAMS__NOTACTIVE
	^-54!

PAMS__NOTALLOCATE
	^-20!

PAMS__NOTDCL
	^-70!

PAMS__NOTJRN
	^-210!

PAMS__NOTPRIMARYQ
	^-266!

PAMS__NOTSECONDARYQ
	^-270!

PAMS__NOTSUPPORTED
	^-268!

PAMS__OBJNOTACTIVE
	^-172!

PAMS__PAMSDOWN
	^-138!

PAMS__PNUMNOEXIST
	^self PAMS__NOTACTIVE!

PAMS__POSSDUPL
	^-158!

PAMS__PREVCALLBUSY
	^-294!

PAMS__PROPAGATE
	^39!

PAMS__PROTOCOL
	^-80!

PAMS__QUECORRUPT
	^-128!

PAMS__RECOVERMODE
	^23!

PAMS__REJECTED
	^-216!

PAMS__REMQUEFAIL
	^-130!

PAMS__RESPQREQ
	^-260!

PAMS__RESRCFAIL
	^-246!

PAMS__RTS_FAILED
	^-232!

PAMS__RTS_SUCCESS
	^19!

PAMS__SAF_DEVICE_FAIL
	^-234!

PAMS__SAF_FAILED
	^-236!

PAMS__SAF_FORCED
	^43!

PAMS__SAF_SUCCESS
	^21!

PAMS__SELRCVACT
	^-240!

PAMS__SENDER_TMO_EXPIRED
	^-162!

PAMS__STALE
	^-304!

PAMS__STATECHANGE
	^-72!

PAMS__STORED
	^31!

PAMS__STUB
	^-160!

PAMS__SUC
	^1!

PAMS__SUCCESS
	^1!

PAMS__TIMEOUT
	^-14!

PAMS__TIMERACT
	^-6!

PAMS__TRACEBACK
	^29!

PAMS__TRUNCATED
	^45!

PAMS__UCBERROR
	^-220!

PAMS__UMA_NA
	^27!

PAMS__UNATTACHEDQ
	^35!

PAMS__WAKEFAIL
	^-4!

PAMS__WRONGDOS
	^-274!

PAMS_ALL_UCBS
	^91!

PAMS_AVAIL_SERVER
	^99!

PAMS_CONNECT_SERVER
	^100!

PAMS_DCL_BY_Q_NAME
	^151!

PAMS_DEAD_LETTER_QUEUE
	^96!

PAMS_DECLARE_SERVER
	^100!

PAMS_DEMO_ARCHIVE
	^106!

PAMS_DEMO_IO_BROADCAST_1
	^5101!

PAMS_DEMO_IO_BROADCAST_2
	^5102!

PAMS_DEMO_IO_SERVER_1
	^101!

PAMS_DEMO_IO_SERVER_2
	^102!

PAMS_DEMO_IO_SERVER_3
	^103!

PAMS_DEMO_IO_SERVER_4
	^104!

PAMS_DEMO_IO_SERVER_MRQ
	^105!

PAMS_ETH_CIRCUIT_1
	^11!

PAMS_ETH_CIRCUIT_2
	^12!

PAMS_ETH_CIRCUIT_3
	^13!

PAMS_ETH_CIRCUIT_4
	^14!

PAMS_ETH_CIRCUIT_5
	^15!

PAMS_FACILITY
	^9!

PAMS_INTERNAL1
	^94!

PAMS_INTERNAL2
	^96!

PAMS_LAST_FAIL
	^-320!

PAMS_LAST_SUCCESS
	^45!

PAMS_MRS_SERVER
	^98!

PAMS_NAME_SERVER
	^97!

PAMS_NULL
	^93!

PAMS_PAMS_LOADER
	^150!

PAMS_PAMS_TRANSPORT
	^100!

PAMS_QTRANSFER_SERVER
	^95!

PAMS_QUEUE_1
	^1!

PAMS_QUEUE_10
	^10!

PAMS_QUEUE_2
	^2!

PAMS_QUEUE_3
	^3!

PAMS_QUEUE_4
	^4!

PAMS_QUEUE_5
	^5!

PAMS_QUEUE_6
	^6!

PAMS_QUEUE_7
	^7!

PAMS_QUEUE_8
	^8!

PAMS_QUEUE_9
	^9!

PAMS_QUEUE_SERVER
	^100!

PAMS_SBS_ETH_CHAN1
	^75!

PAMS_SBS_ETH_CHAN2
	^76!

PAMS_SBS_ETH_CONTROL
	^74!

PAMS_SBS_SERVER
	^99!

PAMS_SCREEN_PROCESS
	^0!

PAMS_SPARE1
	^90!

PAMS_SPARE2
	^92!

PAMS_TEMPORARY_Q
	^0!

PAMS_TIMER_QUEUE
	^92!

PDEL_DEFAULT_JRN
	^0!

PDEL_FORCE_JRN
	^1!

PDEL_MODE_AK_ACK
	^45!

PDEL_MODE_AK_CONF
	^43!

PDEL_MODE_AK_DEQ
	^41!

PDEL_MODE_AK_DQF
	^31!

PDEL_MODE_AK_MEM
	^34!

PDEL_MODE_AK_NET
	^32!

PDEL_MODE_AK_RCM
	^33!

PDEL_MODE_AK_SAF
	^30!

PDEL_MODE_DG
	^1!

PDEL_MODE_DG_LOG
	^0!

PDEL_MODE_NN_DQF
	^36!

PDEL_MODE_NN_MEM
	^39!

PDEL_MODE_NN_NET
	^37!

PDEL_MODE_NN_RCM
	^38!

PDEL_MODE_NN_SAF
	^35!

PDEL_MODE_RTS
	^10!

PDEL_MODE_ST_RECOVER
	^22!

PDEL_MODE_WF_ACK
	^44!

PDEL_MODE_WF_CONF
	^42!

PDEL_MODE_WF_DEQ
	^40!

PDEL_MODE_WF_DQF
	^26!

PDEL_MODE_WF_MEM
	^29!

PDEL_MODE_WF_NET
	^27!

PDEL_MODE_WF_RCM
	^28!

PDEL_MODE_WF_SAF
	^25!

PDEL_MODE_WFQ
	^20!

PDEL_NO_JRN
	^2!

PDEL_UMA_DISC
	^5!

PDEL_UMA_DISCL
	^6!

PDEL_UMA_DLJ
	^2!

PDEL_UMA_DLQ
	^3!

PDEL_UMA_RTS
	^1!

PDEL_UMA_SAF
	^4!

PSEL_AQ
	^-3!

PSEL_AQ_PQ
	^-5!

PSEL_BY_MASK
	^-1!

PSEL_CLASS
	^-2!

PSEL_OPER_ANY
	^0!

PSEL_OPER_EQ
	^1!

PSEL_OPER_GTR
	^3!

PSEL_OPER_GTRE
	^5!

PSEL_OPER_LT
	^4!

PSEL_OPER_LTE
	^6!

PSEL_OPER_NEQ
	^2!

PSEL_ORDER_FIFO
	^0!

PSEL_ORDER_MAX
	^12!

PSEL_ORDER_MIN
	^11!

PSEL_PQ
	^-2!

PSEL_PQ_AQ
	^-4!

PSEL_PQ_CLASS
	^-7!

PSEL_PQ_PRI
	^-8!

PSEL_PQ_TYPE
	^-6!

PSEL_PRI_ANY
	^3!

PSEL_PRI_P0
	^1!

PSEL_PRI_P1
	^2!

PSEL_SOURCE
	^-1!

PSEL_TBL_DNS_CACHE
	^-52!

PSEL_TBL_DNS_HIGH
	^-55!

PSEL_TBL_DNS_LOW
	^-53!

PSEL_TBL_DNS_MED
	^-54!

PSEL_TBL_GRP
	^-51!

PSEL_TBL_PROC
	^-50!

PSEL_TBL_QMA
	^-56!

PSEL_TQ_PQ
	^-10!

PSEL_TQ_PQ_AQ
	^-11!

PSEL_TYPE
	^-3!

PSEL_UCB
	^-9!

PSYM_AK_RESP
	^-71!

PSYM_ASN1
	^2!

PSYM_ATTACH_BY_NAME
	^-210!

PSYM_ATTACH_BY_NUMBER
	^-211!

PSYM_ATTACH_MRQ
	^-207!

PSYM_ATTACH_PQ
	^-200!

PSYM_ATTACH_SQ
	^-201!

PSYM_ATTACH_TEMPORARY
	^-212!

PSYM_BIG_ENDIAN
	^1!

PSYM_CANCEL_SEL_MASK
	^3!

PSYM_DCL_PQ
	^-200!

PSYM_DCL_SQ
	^-201!

PSYM_DETACH_ALL
	^2!

PSYM_IGNORE
	^-230!

PSYM_LINKMGT_ALL_GROUPS
	^-1!

PSYM_LINKMGT_ALREADYUP
	^-6!

PSYM_LINKMGT_CMD_CONNECT
	^102!

PSYM_LINKMGT_CMD_DISCONNECT
	^103!

PSYM_LINKMGT_CMD_ENABLE
	^104!

PSYM_LINKMGT_CMD_INQUIRY
	^101!

PSYM_LINKMGT_CONNECTED
	^303!

PSYM_LINKMGT_DECNET
	^202!

PSYM_LINKMGT_DISABLED
	^304!

PSYM_LINKMGT_LOCAL
	^201!

PSYM_LINKMGT_MSGCONTENT
	^-4!

PSYM_LINKMGT_MSGFMT
	^-2!

PSYM_LINKMGT_NO_TIMER
	^0!

PSYM_LINKMGT_NOCNT
	^302!

PSYM_LINKMGT_NOGROUP
	^-10!

PSYM_LINKMGT_NOPRIV
	^-8!

PSYM_LINKMGT_NOTRANSPORT
	^-14!

PSYM_LINKMGT_NOTSUPPORTED
	^-12!

PSYM_LINKMGT_OPERATIONFAIL
	^-16!

PSYM_LINKMGT_SUCCESS
	^1!

PSYM_LINKMGT_TCPIP
	^203!

PSYM_LINKMGT_UNKNOWN
	^301!

PSYM_LINKMGT_USE_PREVIOUS
	^-1!

PSYM_LITTLE_ENDIAN
	^0!

PSYM_NETWORK_BYTE_ORDER
	^1!

PSYM_NOFLUSH_Q
	^1!

PSYM_OS_TYPE_AIX
	^'I'!

PSYM_OS_TYPE_HPUX
	^'H'!

PSYM_OS_TYPE_IRIX
	^'G'!

PSYM_OS_TYPE_MACINTOSH
	^'A'!

PSYM_OS_TYPE_MSDOS
	^'D'!

PSYM_OS_TYPE_NT
	^'N'!

PSYM_OS_TYPE_OS2
	^'O'!

PSYM_OS_TYPE_OSF1
	^'1'!

PSYM_OS_TYPE_RSX
	^'M'!

PSYM_OS_TYPE_SCO
	^'C'!

PSYM_OS_TYPE_SOLARIS
	^'L'!

PSYM_OS_TYPE_SUNOS
	^'S'!

PSYM_OS_TYPE_SYSV
	^'5'!

PSYM_OS_TYPE_ULTRIX_MIPS
	^'Y'!

PSYM_OS_TYPE_ULTRIX_VAX
	^'X'!

PSYM_OS_TYPE_UNIX
	^'U'!

PSYM_OS_TYPE_UNKNOWN
	^'*'!

PSYM_OS_TYPE_VAXELN
	^'E'!

PSYM_OS_TYPE_VMS
	^'V'!

PSYM_PLATFORM_AIX_RS6000
	^6!

PSYM_PLATFORM_ELN_VAX
	^10!

PSYM_PLATFORM_HP9000_HPUX
	^3!

PSYM_PLATFORM_HPUX_HPPA
	^3!

PSYM_PLATFORM_HPUX_M68K
	^26!

PSYM_PLATFORM_IBM_RS6000_AIX
	^6!

PSYM_PLATFORM_IRIX_MIPS
	^27!

PSYM_PLATFORM_M68K
	^13!

PSYM_PLATFORM_MACINTOSH
	^11!

PSYM_PLATFORM_MACINTOSH_M68K
	^11!

PSYM_PLATFORM_MACINTOSH_POWERPC
	^24!

PSYM_PLATFORM_MOTOROLA_VR32
	^4!

PSYM_PLATFORM_MSDOS
	^8!

PSYM_PLATFORM_MSDOS_X86
	^8!

PSYM_PLATFORM_NT_AXP
	^19!

PSYM_PLATFORM_NT_POWERPC
	^20!

PSYM_PLATFORM_NT_X86
	^16!

PSYM_PLATFORM_OS2
	^7!

PSYM_PLATFORM_OS2_POWERPC
	^25!

PSYM_PLATFORM_OS2_X86
	^7!

PSYM_PLATFORM_OSF1_AXP
	^17!

PSYM_PLATFORM_OSF1_HPPA
	^22!

PSYM_PLATFORM_OSF1_RS6000
	^21!

PSYM_PLATFORM_PDP11_RSX
	^9!

PSYM_PLATFORM_RISC_ULTRIX
	^2!

PSYM_PLATFORM_RSX_PDP11
	^9!

PSYM_PLATFORM_SCO_UNIX
	^12!

PSYM_PLATFORM_SCO_X86
	^12!

PSYM_PLATFORM_SOLARIS_SPARC
	^18!

PSYM_PLATFORM_SOLARIS_X86
	^23!

PSYM_PLATFORM_SPARC_SUNOS
	^5!

PSYM_PLATFORM_SUNOS_SPARC
	^5!

PSYM_PLATFORM_SYSV_M68K
	^13!

PSYM_PLATFORM_SYSV_M88K
	^4!

PSYM_PLATFORM_SYSV_X86
	^28!

PSYM_PLATFORM_ULTRIX_MIPS
	^2!

PSYM_PLATFORM_ULTRIX_VAX
	^1!

PSYM_PLATFORM_UNIX
	^15!

PSYM_PLATFORM_UNKNOWN
	^99!

PSYM_PLATFORM_VAX_ULTRIX
	^1!

PSYM_PLATFORM_VAX_VMS
	^0!

PSYM_PLATFORM_VAXELN
	^10!

PSYM_PLATFORM_VMS_AXP
	^14!

PSYM_PLATFORM_VMS_VAX
	^0!

PSYM_PLATFORM_WINDOWSNT
	^16!

PSYM_QNOT_BADPARAM
	^-2!

PSYM_QNOT_FAIL
	^0!

PSYM_QNOT_SUCCESS
	^1!

PSYM_QTYPE_MRQ
	^3!

PSYM_QTYPE_PQ
	^1!

PSYM_QTYPE_SQ
	^2!

PSYM_QTYPE_UCB
	^4!

PSYM_SCOPE_GLOBAL
	^-203!

PSYM_SCOPE_LOCAL
	^-202!

PSYM_SET_UCB_A
	^-240!

PSYM_SET_UCB_B
	^-241!

PSYM_SET_UCB_C
	^-242!

PSYM_SET_UCB_D
	^-243!

PSYM_SET_UCB_E
	^-244!

PSYM_SET_UCB_F
	^-245!

PSYM_SET_UCB_G
	^-246!

PSYM_SET_UCB_H
	^-247!

PSYM_SET_UCB_I
	^-248!

PSYM_SET_UCB_J
	^-249!

PSYM_SET_UCB_PRI_P0
	^-231!

PSYM_SET_UCB_PRI_P1
	^-232!

PSYM_UNKNOWN
	^3!

PSYM_VAX_BYTE_ORDER
	^0!

PSYM_WF_RESP
	^-70!

SS__NORMAL
	^1!

SS_NORMAL
	^1! !
!DMQConstants categoriesFor: #MSG_CLAS_ETHERNET!constants!public! !
!DMQConstants categoriesFor: #MSG_CLAS_MRS!constants!public! !
!DMQConstants categoriesFor: #MSG_CLAS_PAMS!constants!public! !
!DMQConstants categoriesFor: #MSG_CLAS_TEST_CONTROL!constants!public! !
!DMQConstants categoriesFor: #MSG_CLAS_TEST_DATA!constants!public! !
!DMQConstants categoriesFor: #MSG_CLAS_UCB!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ALLOCATED_SQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_AVAIL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_AVAIL_DEREG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_AVAIL_REG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_AVAIL_REG_REPLY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_COM_SERVER_NAK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DECLARE_SQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEL_STATUS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_ADD!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_CHG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_CLIENT_DOWN!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_CLIENT_UP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_DEL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_INQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_LST!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DEMO_LST_NEXT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DISABLE_NOTIFY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DISABLE_QUOTAS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_DISABLE_STATISTICS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_CONNECT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_CONNECT_COMPLETE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_CONNECT_REJECTED!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_DISCONNECT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_INITIALIZE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_IO_ERROR!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_LISTEN_TIMEOUT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_MESSAGES_LOST!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_PARTNER_DISC!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_PROTOCOL_ERROR!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_E_RUNTIME_ERROR!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ENABLE_NOTIFY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ENABLE_QUOTAS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ENABLE_STATISTICS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_GCP_SHUTDOWN!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_GET_PROC_INFO!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_GET_PROC_INFO_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_GROUP_DETAIL_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_GROUP_DETAIL_RESP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LINK_COMPLETE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LINK_DISABLE_NOTIFY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LINK_ENABLE_NOTIFY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LINK_LOSS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LINK_LOST!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LIST_ALL_CONNECTIONS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LIST_ALL_ENTRIES!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LIST_ALL_ENTRYS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LIST_ALL_GROUPS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_LIST_DCLS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_DISPLAY_CTR!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_DSPLY_QUOTA!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_GRP_TBL_A!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_GRP_TBL_G!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_GRP_TBL_GT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_PROCESS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_RECONNECT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_RESET!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITOR_TERMINATE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MONITORSTOP_ERLOG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MOT_DEREG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_CNF!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_CNF_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_CNF_NAK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_DEBUG_OFF!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_DEBUG_ON!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_DQF_TRANSFER!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_DQF_TRANSFER_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_DQF_TRANSFER_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_SET_DLJ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_SET_DLJ_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_SET_PCJ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MRS_SET_PCJ_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_MSG_STATUS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_ADD!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_ADD_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_DEL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_DEL_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_DUMP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_GBL_ID!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_ID!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_ID_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_LCL_ID!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_NAME!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_NAME_INQ_NAME_REP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_PROCESS_DCL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_PROCESS_EXIT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_CLEANUP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_DETAIL_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_DETAIL_RESP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_STATS_RESET!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_SUMMARY_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_Q_SUMMARY_RESP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_QUEUE_BYTE_QUOTAS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_QUEUE_MESSAGE_QUOTAS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ROUTING_SUMMARY_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_ROUTING_SUMMARY_RESP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_BS_SEQGAP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_DEREG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_DEREG_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_DUMP_TABLES!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_PURGE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_PURGE_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_REG!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_REG_EZ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_REG_EZ_REPLY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_SBS_REG_REPLY!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_ACK!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_DATA!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_SIGNOFF!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_SIGNON!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TEST_STATUS!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TIMER_ADD!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TIMER_CANCEL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_TIMER_EXPIRED!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_CONNECT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_CONNECT_COMPLETE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_DISCONNECT!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_INITIALIZE!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_IO_ERROR!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_MESSAGES_LOST!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_PARTNER_DISC!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UCB_RCV_DATA!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UNAVAIL!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_UNDECLARE_SQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_CLEANUP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_DETAIL_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_DETAIL_RESP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_START!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_STATS_RESET!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_STOP!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_SUMMARY_REQ!constants!public! !
!DMQConstants categoriesFor: #MSG_TYPE_XGROUP_SUMMARY_RESP!constants!public! !
!DMQConstants categoriesFor: #PAMS__ABORT!constants!public! !
!DMQConstants categoriesFor: #PAMS__ACKTMO!constants!public! !
!DMQConstants categoriesFor: #PAMS__AREATOSMALL!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADARGLIST!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADASSIGN!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADASTPARM!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADCMD!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADDECLARE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADDELIVERY!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADFREE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADINITFILE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADJOURNAL!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADLOGIC!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADMSGBUF!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADNAME!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADPARAM!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADPRIORITY!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADPROCNUM!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADQTYPE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADRECEIVE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADRESPQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADSCRIPT!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADSELIDX!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADSEQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADSYNCHNUM!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADTAG!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADTBQHANDLE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADTIME!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADTMPPROC!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADTMPSYNCH!constants!public! !
!DMQConstants categoriesFor: #PAMS__BADUMA!constants!public! !
!DMQConstants categoriesFor: #PAMS__BIGBLKSIZE!constants!public! !
!DMQConstants categoriesFor: #PAMS__BIGMSG!constants!public! !
!DMQConstants categoriesFor: #PAMS__BUFFEROVF!constants!public! !
!DMQConstants categoriesFor: #PAMS__BUSNOTSET!constants!public! !
!DMQConstants categoriesFor: #PAMS__CANCEL!constants!public! !
!DMQConstants categoriesFor: #PAMS__CIRACT!constants!public! !
!DMQConstants categoriesFor: #PAMS__COMMERR!constants!public! !
!DMQConstants categoriesFor: #PAMS__CONFIRMREQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__CONFLICT!constants!public! !
!DMQConstants categoriesFor: #PAMS__CREATEFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__DCLTMPFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__DECLARED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DECNETDEAD!constants!public! !
!DMQConstants categoriesFor: #PAMS__DETACHED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DISC_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DISC_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__DISCL_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DISCL_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__DLJ_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DLJ_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__DLQ_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__DLQ_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__DNSCLASSBAD!constants!public! !
!DMQConstants categoriesFor: #PAMS__DNSDIRFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__DNSFMTBAD!constants!public! !
!DMQConstants categoriesFor: #PAMS__DQF_DEVICE_FAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__DQF_FULL!constants!public! !
!DMQConstants categoriesFor: #PAMS__DUPLQNAME!constants!public! !
!DMQConstants categoriesFor: #PAMS__ENQUEUED!constants!public! !
!DMQConstants categoriesFor: #PAMS__EX_Q_LEN!constants!public! !
!DMQConstants categoriesFor: #PAMS__EXCEEDQUOTA!constants!public! !
!DMQConstants categoriesFor: #PAMS__EXCMAXUNCONF!constants!public! !
!DMQConstants categoriesFor: #PAMS__EXHAUSTBLKS!constants!public! !
!DMQConstants categoriesFor: #PAMS__EXPIRED!constants!public! !
!DMQConstants categoriesFor: #PAMS__FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__FATAL!constants!public! !
!DMQConstants categoriesFor: #PAMS__GROUPNOTSET!constants!public! !
!DMQConstants categoriesFor: #PAMS__IDXTBLFULL!constants!public! !
!DMQConstants categoriesFor: #PAMS__INSQUEFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__INTERNAL!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVACCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVALIDID!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVALIDNUM!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVBUFFPTR!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVFORMAT!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVJH!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVUCBCNTRL!constants!public! !
!DMQConstants categoriesFor: #PAMS__INVUMA!constants!public! !
!DMQConstants categoriesFor: #PAMS__JOURNAL_FAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__JOURNAL_FULL!constants!public! !
!DMQConstants categoriesFor: #PAMS__JOURNAL_ON!constants!public! !
!DMQConstants categoriesFor: #PAMS__LINK_DOWN!constants!public! !
!DMQConstants categoriesFor: #PAMS__LINK_UP!constants!public! !
!DMQConstants categoriesFor: #PAMS__LOGNAME!constants!public! !
!DMQConstants categoriesFor: #PAMS__LOGNAME2!constants!public! !
!DMQConstants categoriesFor: #PAMS__LOGNAME3!constants!public! !
!DMQConstants categoriesFor: #PAMS__LOGNAME4!constants!public! !
!DMQConstants categoriesFor: #PAMS__LOGNAME5!constants!public! !
!DMQConstants categoriesFor: #PAMS__MRQTBLFULL!constants!public! !
!DMQConstants categoriesFor: #PAMS__MRS_RES_EXH!constants!public! !
!DMQConstants categoriesFor: #PAMS__MSGACT!constants!public! !
!DMQConstants categoriesFor: #PAMS__MSGTOBIG!constants!public! !
!DMQConstants categoriesFor: #PAMS__MSGTOSMALL!constants!public! !
!DMQConstants categoriesFor: #PAMS__MSGUNDEL!constants!public! !
!DMQConstants categoriesFor: #PAMS__NAMETOOLONG!constants!public! !
!DMQConstants categoriesFor: #PAMS__NETERROR!constants!public! !
!DMQConstants categoriesFor: #PAMS__NETLINKLOST!constants!public! !
!DMQConstants categoriesFor: #PAMS__NETNOLINK!constants!public! !
!DMQConstants categoriesFor: #PAMS__NO_DQF!constants!public! !
!DMQConstants categoriesFor: #PAMS__NO_SAF!constants!public! !
!DMQConstants categoriesFor: #PAMS__NO_UMA!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOACCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOACL!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOCANSEND!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOLINK!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOMOREJH!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOMOREMSG!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOMRQRESRC!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOMRS!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOOBJECT!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOOPEN!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOPRIV!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOQUOTA!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOSEND!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOSUCHPCJ!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTACTIVE!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTALLOCATE!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTDCL!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTJRN!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTPRIMARYQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTSECONDARYQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__NOTSUPPORTED!constants!public! !
!DMQConstants categoriesFor: #PAMS__OBJNOTACTIVE!constants!public! !
!DMQConstants categoriesFor: #PAMS__PAMSDOWN!constants!public! !
!DMQConstants categoriesFor: #PAMS__PNUMNOEXIST!constants!public! !
!DMQConstants categoriesFor: #PAMS__POSSDUPL!constants!public! !
!DMQConstants categoriesFor: #PAMS__PREVCALLBUSY!constants!public! !
!DMQConstants categoriesFor: #PAMS__PROPAGATE!constants!public! !
!DMQConstants categoriesFor: #PAMS__PROTOCOL!constants!public! !
!DMQConstants categoriesFor: #PAMS__QUECORRUPT!constants!public! !
!DMQConstants categoriesFor: #PAMS__RECOVERMODE!constants!public! !
!DMQConstants categoriesFor: #PAMS__REJECTED!constants!public! !
!DMQConstants categoriesFor: #PAMS__REMQUEFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__RESPQREQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__RESRCFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__RTS_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__RTS_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__SAF_DEVICE_FAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__SAF_FAILED!constants!public! !
!DMQConstants categoriesFor: #PAMS__SAF_FORCED!constants!public! !
!DMQConstants categoriesFor: #PAMS__SAF_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__SELRCVACT!constants!public! !
!DMQConstants categoriesFor: #PAMS__SENDER_TMO_EXPIRED!constants!public! !
!DMQConstants categoriesFor: #PAMS__STALE!constants!public! !
!DMQConstants categoriesFor: #PAMS__STATECHANGE!constants!public! !
!DMQConstants categoriesFor: #PAMS__STORED!constants!public! !
!DMQConstants categoriesFor: #PAMS__STUB!constants!public! !
!DMQConstants categoriesFor: #PAMS__SUC!constants!public! !
!DMQConstants categoriesFor: #PAMS__SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS__TIMEOUT!constants!public! !
!DMQConstants categoriesFor: #PAMS__TIMERACT!constants!public! !
!DMQConstants categoriesFor: #PAMS__TRACEBACK!constants!public! !
!DMQConstants categoriesFor: #PAMS__TRUNCATED!constants!public! !
!DMQConstants categoriesFor: #PAMS__UCBERROR!constants!public! !
!DMQConstants categoriesFor: #PAMS__UMA_NA!constants!public! !
!DMQConstants categoriesFor: #PAMS__UNATTACHEDQ!constants!public! !
!DMQConstants categoriesFor: #PAMS__WAKEFAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS__WRONGDOS!constants!public! !
!DMQConstants categoriesFor: #PAMS_ALL_UCBS!constants!public! !
!DMQConstants categoriesFor: #PAMS_AVAIL_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_CONNECT_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_DCL_BY_Q_NAME!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEAD_LETTER_QUEUE!constants!public! !
!DMQConstants categoriesFor: #PAMS_DECLARE_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_ARCHIVE!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_BROADCAST_1!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_BROADCAST_2!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_SERVER_1!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_SERVER_2!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_SERVER_3!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_SERVER_4!constants!public! !
!DMQConstants categoriesFor: #PAMS_DEMO_IO_SERVER_MRQ!constants!public! !
!DMQConstants categoriesFor: #PAMS_ETH_CIRCUIT_1!constants!public! !
!DMQConstants categoriesFor: #PAMS_ETH_CIRCUIT_2!constants!public! !
!DMQConstants categoriesFor: #PAMS_ETH_CIRCUIT_3!constants!public! !
!DMQConstants categoriesFor: #PAMS_ETH_CIRCUIT_4!constants!public! !
!DMQConstants categoriesFor: #PAMS_ETH_CIRCUIT_5!constants!public! !
!DMQConstants categoriesFor: #PAMS_FACILITY!constants!public! !
!DMQConstants categoriesFor: #PAMS_INTERNAL1!constants!public! !
!DMQConstants categoriesFor: #PAMS_INTERNAL2!constants!public! !
!DMQConstants categoriesFor: #PAMS_LAST_FAIL!constants!public! !
!DMQConstants categoriesFor: #PAMS_LAST_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS_MRS_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_NAME_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_NULL!constants!public! !
!DMQConstants categoriesFor: #PAMS_PAMS_LOADER!constants!public! !
!DMQConstants categoriesFor: #PAMS_PAMS_TRANSPORT!constants!public! !
!DMQConstants categoriesFor: #PAMS_QTRANSFER_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_1!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_10!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_2!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_3!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_4!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_5!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_6!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_7!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_8!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_9!constants!public! !
!DMQConstants categoriesFor: #PAMS_QUEUE_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_SBS_ETH_CHAN1!constants!public! !
!DMQConstants categoriesFor: #PAMS_SBS_ETH_CHAN2!constants!public! !
!DMQConstants categoriesFor: #PAMS_SBS_ETH_CONTROL!constants!public! !
!DMQConstants categoriesFor: #PAMS_SBS_SERVER!constants!public! !
!DMQConstants categoriesFor: #PAMS_SCREEN_PROCESS!constants!public! !
!DMQConstants categoriesFor: #PAMS_SPARE1!constants!public! !
!DMQConstants categoriesFor: #PAMS_SPARE2!constants!public! !
!DMQConstants categoriesFor: #PAMS_TEMPORARY_Q!constants!public! !
!DMQConstants categoriesFor: #PAMS_TIMER_QUEUE!constants!public! !
!DMQConstants categoriesFor: #PDEL_DEFAULT_JRN!constants!public! !
!DMQConstants categoriesFor: #PDEL_FORCE_JRN!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_ACK!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_CONF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_DEQ!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_DQF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_MEM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_NET!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_RCM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_AK_SAF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_DG!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_DG_LOG!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_NN_DQF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_NN_MEM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_NN_NET!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_NN_RCM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_NN_SAF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_RTS!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_ST_RECOVER!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_ACK!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_CONF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_DEQ!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_DQF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_MEM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_NET!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_RCM!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WF_SAF!constants!public! !
!DMQConstants categoriesFor: #PDEL_MODE_WFQ!constants!public! !
!DMQConstants categoriesFor: #PDEL_NO_JRN!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_DISC!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_DISCL!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_DLJ!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_DLQ!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_RTS!constants!public! !
!DMQConstants categoriesFor: #PDEL_UMA_SAF!constants!public! !
!DMQConstants categoriesFor: #PSEL_AQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_AQ_PQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_BY_MASK!constants!public! !
!DMQConstants categoriesFor: #PSEL_CLASS!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_ANY!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_EQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_GTR!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_GTRE!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_LT!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_LTE!constants!public! !
!DMQConstants categoriesFor: #PSEL_OPER_NEQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_ORDER_FIFO!constants!public! !
!DMQConstants categoriesFor: #PSEL_ORDER_MAX!constants!public! !
!DMQConstants categoriesFor: #PSEL_ORDER_MIN!constants!public! !
!DMQConstants categoriesFor: #PSEL_PQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_PQ_AQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_PQ_CLASS!constants!public! !
!DMQConstants categoriesFor: #PSEL_PQ_PRI!constants!public! !
!DMQConstants categoriesFor: #PSEL_PQ_TYPE!constants!public! !
!DMQConstants categoriesFor: #PSEL_PRI_ANY!constants!public! !
!DMQConstants categoriesFor: #PSEL_PRI_P0!constants!public! !
!DMQConstants categoriesFor: #PSEL_PRI_P1!constants!public! !
!DMQConstants categoriesFor: #PSEL_SOURCE!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_DNS_CACHE!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_DNS_HIGH!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_DNS_LOW!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_DNS_MED!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_GRP!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_PROC!constants!public! !
!DMQConstants categoriesFor: #PSEL_TBL_QMA!constants!public! !
!DMQConstants categoriesFor: #PSEL_TQ_PQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_TQ_PQ_AQ!constants!public! !
!DMQConstants categoriesFor: #PSEL_TYPE!constants!public! !
!DMQConstants categoriesFor: #PSEL_UCB!constants!public! !
!DMQConstants categoriesFor: #PSYM_AK_RESP!constants!public! !
!DMQConstants categoriesFor: #PSYM_ASN1!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_BY_NAME!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_BY_NUMBER!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_MRQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_PQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_SQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_ATTACH_TEMPORARY!constants!public! !
!DMQConstants categoriesFor: #PSYM_BIG_ENDIAN!constants!public! !
!DMQConstants categoriesFor: #PSYM_CANCEL_SEL_MASK!constants!public! !
!DMQConstants categoriesFor: #PSYM_DCL_PQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_DCL_SQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_DETACH_ALL!constants!public! !
!DMQConstants categoriesFor: #PSYM_IGNORE!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_ALL_GROUPS!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_ALREADYUP!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_CMD_CONNECT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_CMD_DISCONNECT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_CMD_ENABLE!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_CMD_INQUIRY!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_CONNECTED!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_DECNET!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_DISABLED!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_LOCAL!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_MSGCONTENT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_MSGFMT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NO_TIMER!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NOCNT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NOGROUP!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NOPRIV!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NOTRANSPORT!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_NOTSUPPORTED!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_OPERATIONFAIL!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_TCPIP!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_UNKNOWN!constants!public! !
!DMQConstants categoriesFor: #PSYM_LINKMGT_USE_PREVIOUS!constants!public! !
!DMQConstants categoriesFor: #PSYM_LITTLE_ENDIAN!constants!public! !
!DMQConstants categoriesFor: #PSYM_NETWORK_BYTE_ORDER!constants!public! !
!DMQConstants categoriesFor: #PSYM_NOFLUSH_Q!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_AIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_HPUX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_IRIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_MACINTOSH!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_MSDOS!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_NT!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_OS2!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_OSF1!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_RSX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_SCO!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_SOLARIS!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_SUNOS!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_SYSV!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_ULTRIX_MIPS!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_ULTRIX_VAX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_UNIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_UNKNOWN!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_VAXELN!constants!public! !
!DMQConstants categoriesFor: #PSYM_OS_TYPE_VMS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_AIX_RS6000!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_ELN_VAX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_HP9000_HPUX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_HPUX_HPPA!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_HPUX_M68K!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_IBM_RS6000_AIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_IRIX_MIPS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_M68K!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MACINTOSH!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MACINTOSH_M68K!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MACINTOSH_POWERPC!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MOTOROLA_VR32!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MSDOS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_MSDOS_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_NT_AXP!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_NT_POWERPC!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_NT_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OS2!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OS2_POWERPC!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OS2_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OSF1_AXP!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OSF1_HPPA!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_OSF1_RS6000!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_PDP11_RSX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_RISC_ULTRIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_RSX_PDP11!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SCO_UNIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SCO_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SOLARIS_SPARC!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SOLARIS_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SPARC_SUNOS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SUNOS_SPARC!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SYSV_M68K!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SYSV_M88K!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_SYSV_X86!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_ULTRIX_MIPS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_ULTRIX_VAX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_UNIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_UNKNOWN!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_VAX_ULTRIX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_VAX_VMS!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_VAXELN!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_VMS_AXP!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_VMS_VAX!constants!public! !
!DMQConstants categoriesFor: #PSYM_PLATFORM_WINDOWSNT!constants!public! !
!DMQConstants categoriesFor: #PSYM_QNOT_BADPARAM!constants!public! !
!DMQConstants categoriesFor: #PSYM_QNOT_FAIL!constants!public! !
!DMQConstants categoriesFor: #PSYM_QNOT_SUCCESS!constants!public! !
!DMQConstants categoriesFor: #PSYM_QTYPE_MRQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_QTYPE_PQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_QTYPE_SQ!constants!public! !
!DMQConstants categoriesFor: #PSYM_QTYPE_UCB!constants!public! !
!DMQConstants categoriesFor: #PSYM_SCOPE_GLOBAL!constants!public! !
!DMQConstants categoriesFor: #PSYM_SCOPE_LOCAL!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_A!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_B!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_C!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_D!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_E!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_F!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_G!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_H!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_I!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_J!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_PRI_P0!constants!public! !
!DMQConstants categoriesFor: #PSYM_SET_UCB_PRI_P1!constants!public! !
!DMQConstants categoriesFor: #PSYM_UNKNOWN!constants!public! !
!DMQConstants categoriesFor: #PSYM_VAX_BYTE_ORDER!constants!public! !
!DMQConstants categoriesFor: #PSYM_WF_RESP!constants!public! !
!DMQConstants categoriesFor: #SS__NORMAL!constants!public! !
!DMQConstants categoriesFor: #SS_NORMAL!constants!public! !

!DMQConstants class methodsFor!

current
	Current isNil ifTrue: [ Current := self new ].
	^Current!

uninstall
	Current := nil! !
!DMQConstants class categoriesFor: #current!accessing!public! !
!DMQConstants class categoriesFor: #uninstall!public!removing! !

DMQMessage guid: (GUID fromString: '{78315209-125E-11D5-BE08-00010240D5E2}')!
DMQMessage comment: ''!
!DMQMessage categoriesForClass!Unclassified! !
!DMQMessage methodsFor!

bytes
	^bytes!

bytes: anObject
	bytes := anObject asByteArray!

confirm
	| retval seqNmbr status journalStatus |

	seqNmbr := ULARGE_INTEGER new value: self sequenceNumber.
	status := SDWORD new value: 1.
	journalStatus := BYTE new value: (DMQConstants current PDEL_DEFAULT_JRN).

	retval := DMQLibrary default
			pamsConfirmMsg: seqNmbr asParameter
			confirmationStatus: status asParameter
			forceJ: journalStatus asParameter.
	(retval & 1) = 0 ifTrue: [ self error: 'DMQMessage>>confirm : pams_confirm_msg failed' ]!

initialize
	super initialize.
	self priority: 0!

messageClass
	^messageClass!

messageClass: anInteger
	messageClass := anInteger!

priority
	^priority!

priority: anInteger
	priority := anInteger!

recoverable
	^recoverable!

recoverable: aBoolean
	recoverable := aBoolean!

sequenceNumber
	^sequenceNumber!

sequenceNumber: anInteger
	sequenceNumber := anInteger!

sourceQAddress
	^sourceQAddress!

sourceQAddress: aQAddress
	sourceQAddress := aQAddress!

type
	^type!

type: anInteger
	type := anInteger! !
!DMQMessage categoriesFor: #bytes!accessing!public! !
!DMQMessage categoriesFor: #bytes:!accessing!public! !
!DMQMessage categoriesFor: #confirm!public! !
!DMQMessage categoriesFor: #initialize!public! !
!DMQMessage categoriesFor: #messageClass!accessing!public! !
!DMQMessage categoriesFor: #messageClass:!accessing!public! !
!DMQMessage categoriesFor: #priority!accessing!public! !
!DMQMessage categoriesFor: #priority:!accessing!public! !
!DMQMessage categoriesFor: #recoverable!accessing!public! !
!DMQMessage categoriesFor: #recoverable:!accessing!public! !
!DMQMessage categoriesFor: #sequenceNumber!accessing!public! !
!DMQMessage categoriesFor: #sequenceNumber:!accessing!public! !
!DMQMessage categoriesFor: #sourceQAddress!accessing!public! !
!DMQMessage categoriesFor: #sourceQAddress:!accessing!public! !
!DMQMessage categoriesFor: #type!accessing!public! !
!DMQMessage categoriesFor: #type:!accessing!public! !

!DMQMessage class methodsFor!

new
	^self new: DMQQueue defaultMessageBufferSize!

new: byteCount
	^self basicNew
		bytes: (ByteArray new: byteCount);
		initialize! !
!DMQMessage class categoriesFor: #new!public! !
!DMQMessage class categoriesFor: #new:!public! !

DMQQueue guid: (GUID fromString: '{78315208-125E-11D5-BE08-00010240D5E2}')!
DMQQueue comment: 'A virtual base class for message queues.  User code should use one of this class''s subclasses.'!
!DMQQueue categoriesForClass!Unclassified! !
!DMQQueue methodsFor!

attach: aString mode: anInteger
	"Attach to a named queue."

	| retval |

	self qAddress: QAddress new.
	retval := DMQLibrary default
			pamsAttachQ: aString
			attachMode: anInteger
			queueAttached: self qAddress asParameter.
	self status: (DMQStatus for: retval).
	self status isError
		ifTrue: [ self error: 'DMQQueue>>attach:mode: pams_attach_q failed, retval=', retval printString ]!

defaultMessageBufferSize
	^defaultMessageBufferSize!

defaultMessageBufferSize: anInteger
	defaultMessageBufferSize := anInteger!

detach
	| retval optList optLen msgsFlushed |

	optLen := SDWORD new value: 0.
	msgsFlushed := SDWORD new value: 0.
	optList := SDWORDArray new: 1.

	retval := DMQLibrary default
			pamsDetachQ: self qAddress asParameter
			detachOptList: optList asParameter
			detachOptLen: optLen asParameter
			msgsFlushed: msgsFlushed asParameter.
	self status: (DMQStatus for: retval).
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue>>detach  pams_detach_q failed' ]!

getNextMessage
	^self getNextMessage: self defaultMessageBufferSize priority: 0!

getNextMessage: byteCount priority: anIntegerPriority
	| msg retval priority sourceQ msgClass msgType msgAreaLen lenData selFilter psb showBuffer showBufferSize |

	msg := DMQMessage new: byteCount.
	priority := BYTE new value: anIntegerPriority.
	sourceQ := QAddress new.
	msgClass := SWORD new.
	msgType := SWORD new.
	msgAreaLen := SWORD new value: msg byteCount.
	lenData := SWORD new.
	selFilter := SDWORD new value: self selectionFilter.
	psb := PSB new.
	showBuffer := ShowBuffer new.
	showBufferSize := SDWORD new value: showBuffer size.

	retval := DMQLibrary default
			pamsGetMsg: msg bytes asParameter
			priority: priority asParameter
			source: sourceQ asParameter
			class: msgClass asParameter
			type: msgType asParameter
			msgAreaLen: msgAreaLen asParameter
			lenData: lenData asParameter
			selFilter: selFilter asParamter
			psb: psb asParameter
			showBuffer:  showBuffer asParameter
			showBufferLen:  showBufferSize asParameter.
	self status: (DMQStatus for: retval).
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue>>getNextMessage:priority : pams_get_msg failed' ].

	msg
		messageClass: msgClass value;
		sequenceNumber: psb seqNumber value;
		sourceQAddress: sourceQ;
		type: msgType value;
		recoverable: ((psb delPsbStatus value = DMQConstants current PAMS__CONFIRMREQ) |
				    (psb delPsbStatus value = DMQConstants current PAMS__POSSDUPL)).
	^msg!

initialize
	super initialize.
	self defaultMessageBufferSize: self class defaultMessageBufferSize.
	self selectionFilter: 0!

qAddress
	^qAddress!

qAddress: aQAddress
	"Private"
	qAddress := aQAddress!

selectionFilter
	^selectionFilter!

selectionFilter: anInteger
	selectionFilter := anInteger!

send: aDmqMessage to: aStringQueueName
	self
		send: aDmqMessage
		to: aStringQueueName
		deliveryMode: DMQConstants current PDEL_MODE_WF_SAF
		uma: DMQConstants current PDEL_UMA_SAF
		timeout: 0!

send: aDmqMessage to: aStringQueueName
		deliveryMode: anIntegerMode uma: anIntegerUma timeout: anIntegerTimeout
	| deliveryMode uma timeout priority targetQueue retval messageClass messageLength psb messageType |

	deliveryMode := BYTE new value: anIntegerMode.
	uma := BYTE new value: anIntegerUma.
	timeout := SDWORD new value: anIntegerTimeout.
	priority := BYTE new value: 0.
	messageClass := SWORD new value: aDmqMessage messageClass.
	messageType := SWORD new value: aDmqMessage type.
	messageLength := SWORD new value: aDmqMessage bytes size.
	psb := PSB new.

	targetQueue := self class addressOfQueueNamed: aStringQueueName.

	retval := DMQLibrary default
			pamsPutMsg: aDmqMessage bytes asParameter
			priority: priority asParameter
			target: targetQueue asParameter
			class: messageClass asParameter
			type: messageType asParameter
			delivery: deliveryMode asParameter
			msgSize: messageLength asParameter
			timeout: timeout asParameter
			psb: psb asParameter
			uma: uma asParameter
			respQ: nil asParameter.
	self status: (DMQStatus for: retval).
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue>>send:to:deliveryMode:uma:timeout failed' ]!

status
	^status!

status: aDMQStatus
	status := aDMQStatus!

waitForNextMessage
		^self waitForNextMessage: self defaultMessageBufferSize priority: 0 timeout: 0!

waitForNextMessage: byteCount priority: anIntegerPriority timeout: anIntegerTimeout
	| msg retval priority sourceQ msgClass msgType msgAreaLen
	  lenData selFilter psb showBuffer showBufferSize timeout |

	msg := DMQMessage new: byteCount.
	priority := BYTE new value: anIntegerPriority.
	sourceQ := QAddress new.
	msgClass := SWORD new.
	msgType := SWORD new.
	msgAreaLen := SWORD new value: msg byteCount.
	lenData := SWORD new.
	selFilter := SDWORD new value: self selectionFilter.
	psb := PSB new.
	showBuffer := ShowBuffer new.
	showBufferSize := SDWORD new value: showBuffer size.
	timeout := SDWORD new value: anIntegerTimeout.

	retval := DMQLibrary default
			pamsGetMsgW: msg bytes asParameter
			priority: priority asParameter
			source: sourceQ asParameter
			class: msgClass asParameter
			type: msgType asParameter
			msgAreaLen: msgAreaLen asParameter
			lenData: lenData asParameter
			timeout: timeout asParameter
			selFilter: selFilter asParamter
			psb: psb asParameter
			showBuffer:  showBuffer asParameter
			showBufferLen:  showBufferSize asParameter.
	self status: (DMQStatus for: retval).
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue>>getNextMessage:priority : pams_get_msg failed' ].

	msg
		messageClass: msgClass value;
		sequenceNumber: psb seqNumber value;
		sourceQAddress: sourceQ;
		type: msgType value;
		recoverable: ((psb delPsbStatus value = DMQConstants current PAMS__CONFIRMREQ) |
				    (psb delPsbStatus value = DMQConstants current PAMS__POSSDUPL)).
	^msg! !
!DMQQueue categoriesFor: #attach:mode:!operations!private! !
!DMQQueue categoriesFor: #defaultMessageBufferSize!accessing!public! !
!DMQQueue categoriesFor: #defaultMessageBufferSize:!accessing!public! !
!DMQQueue categoriesFor: #detach!operations!public! !
!DMQQueue categoriesFor: #getNextMessage!operations!public! !
!DMQQueue categoriesFor: #getNextMessage:priority:!operations!public! !
!DMQQueue categoriesFor: #initialize!initialization!public! !
!DMQQueue categoriesFor: #qAddress!accessing!public! !
!DMQQueue categoriesFor: #qAddress:!accessing!private! !
!DMQQueue categoriesFor: #selectionFilter!accessing!public! !
!DMQQueue categoriesFor: #selectionFilter:!accessing!public! !
!DMQQueue categoriesFor: #send:to:!operations!public! !
!DMQQueue categoriesFor: #send:to:deliveryMode:uma:timeout:!operations!public! !
!DMQQueue categoriesFor: #status!accessing!public! !
!DMQQueue categoriesFor: #status:!accessing!private! !
!DMQQueue categoriesFor: #waitForNextMessage!operations!public! !
!DMQQueue categoriesFor: #waitForNextMessage:priority:timeout:!operations!public! !

!DMQQueue class methodsFor!

addressOfQueueNamed: aString
	"Answers a QAddress for the given name.  Throws an exception if a queue with the given name
	 cannot be found."

	| qNameLen qAddr retval status |

	qNameLen := SDWORD new value: aString size.
	qAddr := QAddress new.
	retval := DMQLibrary default
				pamsLocateQ: aString asParameter
				qNameLen: qNameLen asParameter
				qAddress: qAddr asParameter.
	status := DMQStatus for: retval.
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue class>>addressOfQueueNamed: pams_locate_q failed' ].
	^qAddr!

defaultMessageBufferSize
	^4096!

disconnect
	| retval status |

	retval := DMQLibrary default pamsExit.
	(retval & 1) = 0 ifTrue: [ self error: 'DMQQueue class>>disconnect: pams_exit failed' ]!

new
	^super new initialize! !
!DMQQueue class categoriesFor: #addressOfQueueNamed:!public! !
!DMQQueue class categoriesFor: #defaultMessageBufferSize!public! !
!DMQQueue class categoriesFor: #disconnect!public! !
!DMQQueue class categoriesFor: #new!public! !

DMQStatus guid: (GUID fromString: '{7831520A-125E-11D5-BE08-00010240D5E2}')!
DMQStatus comment: ''!
!DMQStatus categoriesForClass!Unclassified! !
!DMQStatus methodsFor!

code
	^code!

code: anInteger
	code := anInteger!

getText
	| buflen retlen retval |

	buflen := SDWORD new value: 256.
	self severity: SDWORD new.
	self text: (String new: buflen value).
	retlen := SDWORD new.
	retval := 0.

	retval := DMQLibrary default
			pamsStatusText: code severity: severity buffer: text buflen: buflen retlen: retlen.
	(retval & 1) = 0
		ifTrue: [ self error: 'pams_status_text failed in DMQStatus>>getText' ]!

isError
	^(self code & 1) = 0!

severity
	severity isNil ifTrue: [ self getText ].
	^severity!

severity: anSDWORD
	severity := anSDWORD!

text
	text isNil ifTrue: [ self getText ].
	^text!

text: aString
	text := aString! !
!DMQStatus categoriesFor: #code!public! !
!DMQStatus categoriesFor: #code:!private! !
!DMQStatus categoriesFor: #getText!private! !
!DMQStatus categoriesFor: #isError!public! !
!DMQStatus categoriesFor: #severity!public! !
!DMQStatus categoriesFor: #severity:!private! !
!DMQStatus categoriesFor: #text!public! !
!DMQStatus categoriesFor: #text:!private! !

!DMQStatus class methodsFor!

clear
	InstanceDictionary := nil!

for: anInteger
	"Determine if an instance of this class exists for the given code - if not, create it and
	 save it in InstanceDictionary."

	^self instanceDictionary at: anInteger ifAbsentPut: [ self basicNew code: anInteger ]!

instanceDictionary
	InstanceDictionary isNil ifTrue: [ InstanceDictionary := Dictionary new ].
	^InstanceDictionary!

new
	self error: 'Use DMQStatus class>>for: to create instances'! !
!DMQStatus class categoriesFor: #clear!public! !
!DMQStatus class categoriesFor: #for:!public! !
!DMQStatus class categoriesFor: #instanceDictionary!public! !
!DMQStatus class categoriesFor: #new!public! !

DMQPermanentQueue guid: (GUID fromString: '{0F41C131-1303-11D5-BE08-00010240D5E2}')!
DMQPermanentQueue comment: ''!
!DMQPermanentQueue categoriesForClass!Unclassified! !
!DMQPermanentQueue methodsFor!

attachByName: aString
	"Attach to a named queue."

	^self attach: aString mode: DMQConstants current PSYM_ATTACH_BY_NAME!

attachByNumber: aString
	"Attach to a queue by number.  Queue numbers are passed as a string of up to
	 three characters specifying the queue number."

	^self attach: aString mode: DMQConstants current PSYM_ATTACH_BY_NUMBER! !
!DMQPermanentQueue categoriesFor: #attachByName:!public! !
!DMQPermanentQueue categoriesFor: #attachByNumber:!public! !

DMQTemporaryQueue guid: (GUID fromString: '{0F41C132-1303-11D5-BE08-00010240D5E2}')!
DMQTemporaryQueue comment: ''!
!DMQTemporaryQueue categoriesForClass!Unclassified! !
!DMQTemporaryQueue methodsFor!

attach
	"Attach to a temporary queue."

	^self attach: '' mode: DMQConstants current PSYM_ATTACH_TEMPORARY! !
!DMQTemporaryQueue categoriesFor: #attach!public! !

DMQLibrary guid: (GUID fromString: '{78315201-125E-11D5-BE08-00010240D5E2}')!
DMQLibrary comment: ''!
!DMQLibrary categoriesForClass!Unclassified! !
!DMQLibrary methodsFor!

basicPamsAttachQ: attachMode queueAttached: queueAttached queueType: queueType queueName: queueName
	queueNameLen: queueNameLen nameSpaceList: nameSpaceList nameSpaceListLen: nameSpaceListLen
	nullArg1: nullArg1 nullArg2: nullArg2 nullArg3: nullArg3
	<stdcall: sdword pams_attach_q SDWORD* QAddress* SDWORD* lpstr SDWORD*
							SDWORDArray* SDWORD* lpvoid lpvoid lpvoid>
	^self invalidCall!

basicPamsGetMsg: msgArea priority: priority source: source class: class type: type msgAreaLen: msgAreaLen
		lenData: lenData selFilter: selFilter psb: psb showBuffer: showBuffer showBufferLen: showBufferLen
		nullArg1: nullArg1 nullArg2: nullArg2 nullArg3: nullArg3
	<stdcall: sdword pams_get_msg lpvoid BYTE* QAddress* SWORD* SWORD* SWORD* SWORD*
							SDWORD* PSB* ShowBuffer* SDWORD* lpvoid lpvoid lpvoid>
	^self invalidCall!

basicPamsGetMsgW: msgArea priority: priority source: source class: class type: type msgAreaLen: msgAreaLen
		lenData: lenData timeout: timeout selFilter: selFilter psb: psb showBuffer: showBuffer
		showBufferLen: showBufferLen nullArg1: nullArg1 nullArg2: nullArg2 nullArg3: nullArg3
	<stdcall: sdword pams_get_msg lpvoid BYTE* QAddress* SWORD* SWORD* SWORD* SWORD* SDWORD*
							SDWORD* PSB* ShowBuffer* SDWORD* lpvoid lpvoid lpvoid>
	^self invalidCall!

basicPamsLocateQ: qName qNameLen: qNameLen qAddress: qAddress waitMode: waitMode
		reqId: reqId respQ: respQ nameSpaceList: nameSpaceList
		nameSpaceListLen: nameSpaceListLen nullArg: nullArg
	<stdcall: sdword pams_locate_q lpstr SDWORD* QAddress* SDWORD* SDWORD* SDWORD*
							SDWORD* SDWORD* lpstr>
	^self invalidCall!

basicPamsPutMsg: msgArea priority: priority target: target class: class type: type delivery: delivery
			msgSize: msgSize timeout: timeout psb: psb uma: uma respQ: respQ
			nullArg1: nullArg1 nullArg2: nullArg2 nullArg3: nullArg3
	<stdcall: sdword pams_put_msg lpvoid BYTE* QAddress* SWORD* SWORD* BYTE*
							SWORD* SDWORD* PSB* BYTE* QAddress*
							lpvoid lpvoid lpvoid>
	^self invalidCall!

pamsAttachQ: aString attachMode: anIntegerAttachMode queueAttached: aQAddress 
	| mode qType queueNameLen nameSpaceListLen |

	mode := SDWORD new value: anIntegerAttachMode.
	qType := SDWORD new value: DMQConstants current PSYM_ATTACH_PQ.
	queueNameLen := SDWORD new value: aString size.
	nameSpaceListLen := SDWORD new value: 0.

	^self basicPamsAttachQ: mode asParameter
		queueAttached: aQAddress asParameter
		queueType: qType asParameter
		queueName: aString asParameter
		queueNameLen: queueNameLen asParameter
		nameSpaceList: nil asParameter
		nameSpaceListLen: nameSpaceListLen asParameter
		nullArg1: nil asParameter
		nullArg2: nil asParameter
		nullArg3: nil asParameter!

pamsAttachQ: attachMode queueAttached: queueAttached queueType: queueType queueName: queueName
	queueNameLen: queueNameLen nameSpaceList: nameSpaceList nameSpaceListLen: nameSpaceListLen
	^self basicPamsAttachQ: attachMode queueAttached: queueAttached queueType: queueType
		queueName: queueName queueNameLen: queueNameLen nameSpaceList: nameSpaceList
		nameSpaceListLen: nameSpaceListLen nullArg1: nil nullArg2: nil nullArg3: nil
!

pamsAttachQ: queueName queueNameLen: queueNameLen attachMode: attachMode queueType: queueType
	nameSpaceList: nameSpaceList nameSpaceListLen: nameSpaceListLen queueAttached: queueAttached

	^self basicPamsAttachQ: attachMode queueAttached: queueAttached queueType: queueType
		queueName: queueName queueNameLen: queueNameLen nameSpaceList: nameSpaceList
		nameSpaceListLen: nameSpaceListLen nullArg1: nil nullArg2: nil nullArg3: nil
!

pamsCancelGet: selFilter
	<stdcall: sdword pams_cancel_get SDWORD*>
	^self invalidCall!

pamsCancelSelect: indexHandle
	<stdcall: sdword pams_cancel_select SDWORD*>
	^self invalidCall!

pamsCancelTimer: timerId
	<stdcall: sdword pams_cancel_timer SDWORD*>
	^self invalidCall!

pamsCloseJrn: jrnHandle
	<stdcall: sdword pams_close_jrn SDWORD*>
	^self invalidCall!

pamsConfirmMsg: msgSeqNum confirmationStatus: confirmationStatus forceJ: forceJ
	<stdcall: sdword pams_confirm_msg ULARGE_INTEGER* SDWORD* BYTE*>
	^self invalidCall!

pamsDetachQ: qAddress detachOptList: detachOptList detachOptLen: detachOptLen msgsFlushed: msgsFlushed
	<stdcall: sdword pams_detach_q QAddress* SDWORD* SDWORD* SDWORD*>
	^self invalidCall!

pamsExit
	<stdcall: sdword pams_exit>
	^self invalidCall!

pamsGetMsg: msgArea priority: priority source: source class: class type: type msgAreaLen: msgAreaLen
		lenData: lenData selFilter: selFilter psb: psb showBuffer: showBuffer showBufferLen: showBufferLen
	^self basicPamsGetMsg: msgArea priority: priority source: source class: class type: type
			msgAreaLen: msgAreaLen lenData: lenData selFilter: selFilter
			psb: psb showBuffer: showBuffer showBufferLen: showBufferLen nullArg1: nil
			nullArg2: nil nullArg3: nil!

pamsGetMsgW: msgArea priority: priority source: source class: class type: type msgAreaLen: msgAreaLen
		lenData: lenData timeout: timeout
	^self basicPamsGetMsgW: msgArea priority: priority source: source class: class type: type
			msgAreaLen: msgAreaLen lenData: lenData timeout: timeout selFilter: nil
			psb: nil showBuffer: nil showBufferLen: nil nullArg1: nil
			nullArg2: nil nullArg3: nil!

pamsGetMsgW: msgArea priority: priority source: source class: class type: type msgAreaLen: msgAreaLen
		lenData: lenData timeout: timeout selFilter: selFilter psb: psb showBuffer: showBuffer
		showBufferLen: showBufferLen
	^self basicPamsGetMsgW: msgArea priority: priority source: source class: class type: type
			msgAreaLen: msgAreaLen lenData: lenData timeout: timeout selFilter: selFilter
			psb: psb showBuffer: showBuffer showBufferLen: showBufferLen nullArg1: nil
			nullArg2: nil nullArg3: nil!

pamsLocateQ: qName qNameLen: qNameLen qAddress: qAddress
	^self basicPamsLocateQ: qName qNameLen: qNameLen qAddress: qAddress waitMode: nil
					reqId: nil respQ: nil nameSpaceList: nil
					nameSpaceListLen: nil nullArg: nil!

pamsLocateQ: qName qNameLen: qNameLen qAddress: qAddress waitMode: waitMode
		reqId: reqId respQ: respQ nameSpaceList: nameSpaceList
		nameSpaceListLen: nameSpaceListLen
	^self basicPamsLocateQ: qName qNameLen: qNameLen qAddress: qAddress waitMode: waitMode
					reqId: reqId respQ: respQ nameSpaceList: nameSpaceList
					nameSpaceListLen: nameSpaceListLen nullArg: nil!

pamsOpenJrn: jrnFileSpec jrnFilenameLen: jrnFilenameLen jrnHandle: jrnHandle
	<stdcall: sdword pams_open_jrn lpstr SWORD* SDWORD*>
	^self invalidCall!

pamsPutMsg: msgArea priority: priority target: target class: class type: type delivery: delivery
			msgSize: msgSize timeout: timeout psb: psb uma: uma respQ: respQ
	^self basicPamsPutMsg: msgArea priority: priority target: target class: class type: type delivery: delivery
			msgSize: msgSize timeout: timeout psb: psb uma: uma respQ: respQ
			nullArg1: nil asParameter nullArg2: nil asParameter nullArg3: nil asParameter!

pamsReadJrn: jrnHandle msgArea: msgArea priority: priority source: source class: class type: type
		msgAreaLen: msgAreaLen lenData: lenData target: target writeTime: writeTime
		confVal: confVal msgSeqNum: msgSeqNum mrsStatus: mrsStatus
	<stdcall: sdword pams_read_jrn SDWORD* lpstr lpstr QAddress* SWORD* SWORD* SWORD* SWORD*
							QAddress* DWORD* SDWORD* DWORD* SDWORD*>
	^self invalidCall!

pamsSetSelect: selectionArray numMasks: numMasks indexHandle: indexHandle
	<stdcall: sdword pams_set_select SelectionArrayComponent* SWORD* SDWORD*>
	^self invalidCall!

pamsSetTimer: timeId timerFormat: timerFormat pTimeout: pTimeout sTimeout: sTimeout
	<stdcall: sdword pams_set_timer SDWORD* lpstr SDWORD* ULARGE_INTEGER*>
	^self invalidCall!

pamsStatusText: code severity: severity buffer: buffer buflen: buflen retlen: retlen
	<stdcall: sdword pams_status_text SDWORD* SDWORD* lpstr SDWORD* SDWORD*>
	^self invalidCall!

putilShowPending: count inQList: inQList outPendList: outPendList
	<stdcall: sdword putil_show_pending SDWORD* SWORDArray* SDWORDArray*>
	^self invalidCall! !
!DMQLibrary categoriesFor: #basicPamsAttachQ:queueAttached:queueType:queueName:queueNameLen:nameSpaceList:nameSpaceListLen:nullArg1:nullArg2:nullArg3:!private! !
!DMQLibrary categoriesFor: #basicPamsGetMsg:priority:source:class:type:msgAreaLen:lenData:selFilter:psb:showBuffer:showBufferLen:nullArg1:nullArg2:nullArg3:!private! !
!DMQLibrary categoriesFor: #basicPamsGetMsgW:priority:source:class:type:msgAreaLen:lenData:timeout:selFilter:psb:showBuffer:showBufferLen:nullArg1:nullArg2:nullArg3:!private! !
!DMQLibrary categoriesFor: #basicPamsLocateQ:qNameLen:qAddress:waitMode:reqId:respQ:nameSpaceList:nameSpaceListLen:nullArg:!private! !
!DMQLibrary categoriesFor: #basicPamsPutMsg:priority:target:class:type:delivery:msgSize:timeout:psb:uma:respQ:nullArg1:nullArg2:nullArg3:!private! !
!DMQLibrary categoriesFor: #pamsAttachQ:attachMode:queueAttached:!public! !
!DMQLibrary categoriesFor: #pamsAttachQ:queueAttached:queueType:queueName:queueNameLen:nameSpaceList:nameSpaceListLen:!public! !
!DMQLibrary categoriesFor: #pamsAttachQ:queueNameLen:attachMode:queueType:nameSpaceList:nameSpaceListLen:queueAttached:!public! !
!DMQLibrary categoriesFor: #pamsCancelGet:!public! !
!DMQLibrary categoriesFor: #pamsCancelSelect:!public! !
!DMQLibrary categoriesFor: #pamsCancelTimer:!public! !
!DMQLibrary categoriesFor: #pamsCloseJrn:!public! !
!DMQLibrary categoriesFor: #pamsConfirmMsg:confirmationStatus:forceJ:!public! !
!DMQLibrary categoriesFor: #pamsDetachQ:detachOptList:detachOptLen:msgsFlushed:!public! !
!DMQLibrary categoriesFor: #pamsExit!public! !
!DMQLibrary categoriesFor: #pamsGetMsg:priority:source:class:type:msgAreaLen:lenData:selFilter:psb:showBuffer:showBufferLen:!public! !
!DMQLibrary categoriesFor: #pamsGetMsgW:priority:source:class:type:msgAreaLen:lenData:timeout:!public! !
!DMQLibrary categoriesFor: #pamsGetMsgW:priority:source:class:type:msgAreaLen:lenData:timeout:selFilter:psb:showBuffer:showBufferLen:!public! !
!DMQLibrary categoriesFor: #pamsLocateQ:qNameLen:qAddress:!public! !
!DMQLibrary categoriesFor: #pamsLocateQ:qNameLen:qAddress:waitMode:reqId:respQ:nameSpaceList:nameSpaceListLen:!public! !
!DMQLibrary categoriesFor: #pamsOpenJrn:jrnFilenameLen:jrnHandle:!public! !
!DMQLibrary categoriesFor: #pamsPutMsg:priority:target:class:type:delivery:msgSize:timeout:psb:uma:respQ:!public! !
!DMQLibrary categoriesFor: #pamsReadJrn:msgArea:priority:source:class:type:msgAreaLen:lenData:target:writeTime:confVal:msgSeqNum:mrsStatus:!public! !
!DMQLibrary categoriesFor: #pamsSetSelect:numMasks:indexHandle:!public! !
!DMQLibrary categoriesFor: #pamsSetTimer:timerFormat:pTimeout:sTimeout:!public! !
!DMQLibrary categoriesFor: #pamsStatusText:severity:buffer:buflen:retlen:!public! !
!DMQLibrary categoriesFor: #putilShowPending:inQList:outPendList:!public! !

!DMQLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^'dmqcl32'!

generateConstants
	"Generate a Smalltalk class containing methods for constants defined by DMQ.  Code
	 commented out to simplify prerequisites."

	"PlainConstantsClassGenerator new
		generateClass: 'DMQConstants'
		from: (Array with: 'D:\DMQ320\Include\P_PROCES.H'
				   with: 'D:\DMQ320\Include\P_RETURN.H'
				   with: 'D:\DMQ320\Include\P_SYMBOL.H'
				   with: 'D:\DMQ320\Include\P_TYPECL.H')"! !
!DMQLibrary class categoriesFor: #fileName!public! !
!DMQLibrary class categoriesFor: #generateConstants!public! !

PSB guid: (GUID fromString: '{78315203-125E-11D5-BE08-00010240D5E2}')!
PSB comment: ''!
!PSB categoriesForClass!Unclassified! !
!PSB methodsFor!

callDependent
	"Answer the receiver's callDependent field as a Smalltalk object."

	^(bytes swordAtOffset: 2)!

callDependent: anObject
	"Set the receiver's callDependent field to the value of anObject."

	bytes swordAtOffset: 2 put: anObject!

delPsbStatus
	"Answer the receiver's delPsbStatus field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

delPsbStatus: anObject
	"Set the receiver's delPsbStatus field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

psbReserved
	"Answer the receiver's psbReserved field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress + 20) length: 6 elementClass: WORD!

psbReserved: anObject
	"Set the receiver's psbReserved field to the value of anObject."

	| size |
	size := anObject byteSize min: (6 * 0).
	anObject replaceBytesOf: bytes from: 21 to: 20 + size startingAt: 1!

seqNumber
	"Answer the receiver's seqNumber field as a Smalltalk object."

	^(bytes qwordAtOffset: 8)!

seqNumber: anObject
	"Set the receiver's seqNumber field to the value of anObject."

	bytes qwordAtOffset: 8 put: anObject!

typeOfPsb
	"Answer the receiver's typeOfPsb field as a Smalltalk object."

	^(bytes swordAtOffset: 0)!

typeOfPsb: anObject
	"Set the receiver's typeOfPsb field to the value of anObject."

	bytes swordAtOffset: 0 put: anObject!

umaPsbStatus
	"Answer the receiver's umaPsbStatus field as a Smalltalk object."

	^(bytes sdwordAtOffset: 16)!

umaPsbStatus: anObject
	"Set the receiver's umaPsbStatus field to the value of anObject."

	bytes sdwordAtOffset: 16 put: anObject! !
!PSB categoriesFor: #callDependent!**compiled accessors**!public! !
!PSB categoriesFor: #callDependent:!**compiled accessors**!public! !
!PSB categoriesFor: #delPsbStatus!**compiled accessors**!public! !
!PSB categoriesFor: #delPsbStatus:!**compiled accessors**!public! !
!PSB categoriesFor: #psbReserved!**compiled accessors**!public! !
!PSB categoriesFor: #psbReserved:!**compiled accessors**!public! !
!PSB categoriesFor: #seqNumber!**compiled accessors**!public! !
!PSB categoriesFor: #seqNumber:!**compiled accessors**!public! !
!PSB categoriesFor: #typeOfPsb!**compiled accessors**!public! !
!PSB categoriesFor: #typeOfPsb:!**compiled accessors**!public! !
!PSB categoriesFor: #umaPsbStatus!**compiled accessors**!public! !
!PSB categoriesFor: #umaPsbStatus:!**compiled accessors**!public! !

!PSB class methodsFor!

defineFields
	"struct PSB {
		short    type_of_psb;
		short    call_dependent;
		int32    del_psb_status;
		uint32   seq_number[2];
		int32    uma_psb_status;
		short    psb_reserved[6]};"

	self
		defineField: #typeOfPsb type: SWORDField new;
		defineField: #callDependent type: SWORDField new;
		defineField: #delPsbStatus type: SDWORDField new;
		defineField: #seqNumber type: QWORDField new;
		defineField: #umaPsbStatus  type: SDWORDField new;
		defineField: #psbReserved type: (StructureArrayField type: WORD length: 6)! !
!PSB class categoriesFor: #defineFields!public! !

QAddress guid: (GUID fromString: '{78315202-125E-11D5-BE08-00010240D5E2}')!
QAddress comment: ''!
!QAddress categoriesForClass!Unclassified! !
!QAddress class methodsFor!

defineFields
	"typedef union {
	   uint32   all;
	   struct {
	      u_short queue;
	      u_short group;
	   } au;
	} q_address;"

	self
		defineField: #all type: DWORDField new offset: 0;
		defineField: #queue type: WORDField new offset: 0;
		defineField: #group type: WORDField new offset: 2
! !
!QAddress class categoriesFor: #defineFields!public! !

SelectionArrayComponent guid: (GUID fromString: '{78315205-125E-11D5-BE08-00010240D5E2}')!
SelectionArrayComponent comment: ''!
!SelectionArrayComponent categoriesForClass!Unclassified! !
!SelectionArrayComponent class methodsFor!

defineFields
	"typedef struct _selection_array_component {
		int32    queue;
		int32    priority;
		int32    key_1_offset;
		int32    key_1_size;
		int32    key_1_value;
		int32    key_1_oper;
		int32    key_2_offset;
		int32    key_2_size;
		int32    key_2_value;
		int32    key_2_oper;
		int32    order_offset;
		int32    order_size;
		int32    order_order} selection_array_component;"

	self
		defineField: #queue type: SDWORDField new;
		defineField: #priority type: SDWORDField new;
		defineField: #key1Offset type: SDWORDField new;
		defineField: #key1Size type: SDWORDField new;
		defineField: #key1Value type: SDWORDField new;
		defineField: #key1Oper type: SDWORDField new;
		defineField: #key2Offset type: SDWORDField new;
		defineField: #key2Size type: SDWORDField new;
		defineField: #key2Value type: SDWORDField new;
		defineField: #key2Oper type: SDWORDField new;
		defineField: #orderOffset type: SDWORDField new;
		defineField: #orderSize type: SDWORDField new;
		defineField: #orderOrder type: SDWORDField new! !
!SelectionArrayComponent class categoriesFor: #defineFields!public! !

ShowBuffer guid: (GUID fromString: '{78315204-125E-11D5-BE08-00010240D5E2}')!
ShowBuffer comment: ''!
!ShowBuffer categoriesForClass!Unclassified! !
!ShowBuffer class methodsFor!

defineFields
	"struct show_buffer { 
		int32    version;
		int32    transfer_status;
		int32    transfer_size;
		int32    reserved[7];
		int32    target;
		int32    original_target;
		int32    source;
		int32    original_source;
		int32    delivery;
		int32    priority;
		int32    endian };"

	self
		defineField: #version type: SDWORDField new;
		defineField: #transferStatus type: SDWORDField new;
		defineField: #transferSize type: SDWORDField new;
		defineField: #reserved type: (StructureArrayField type: SDWORD length: 7);
		defineField: #target  type: SDWORDField new;
		defineField: #originalTarget type: SDWORDField new;
		defineField: #source type: SDWORDField new;
		defineField: #originalSource type: SDWORDField new;
		defineField: #delivery type: SDWORDField new;
		defineField: #priority type: SDWORDField new;
		defineField: #endian type: SDWORDField new! !
!ShowBuffer class categoriesFor: #defineFields!public! !

"Binary Globals"!

"Resources"!

