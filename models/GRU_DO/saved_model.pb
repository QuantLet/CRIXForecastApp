¶±&
™э
8
Const
output"dtype"
valuetensor"
dtypetype

NoOp
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetypeИ
Њ
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring И
q
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshapeИ"serve*2.2.02v2.2.0-rc4-8-g2b96f3662b8±Ц%
x
dense_1/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namedense_1/kernel
q
"dense_1/kernel/Read/ReadVariableOpReadVariableOpdense_1/kernel*
_output_shapes

: *
dtype0
p
dense_1/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_1/bias
i
 dense_1/bias/Read/ReadVariableOpReadVariableOpdense_1/bias*
_output_shapes
:*
dtype0
l
RMSprop/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_nameRMSprop/iter
e
 RMSprop/iter/Read/ReadVariableOpReadVariableOpRMSprop/iter*
_output_shapes
: *
dtype0	
n
RMSprop/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/decay
g
!RMSprop/decay/Read/ReadVariableOpReadVariableOpRMSprop/decay*
_output_shapes
: *
dtype0
~
RMSprop/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *&
shared_nameRMSprop/learning_rate
w
)RMSprop/learning_rate/Read/ReadVariableOpReadVariableOpRMSprop/learning_rate*
_output_shapes
: *
dtype0
t
RMSprop/momentumVarHandleOp*
_output_shapes
: *
dtype0*
shape: *!
shared_nameRMSprop/momentum
m
$RMSprop/momentum/Read/ReadVariableOpReadVariableOpRMSprop/momentum*
_output_shapes
: *
dtype0
j
RMSprop/rhoVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/rho
c
RMSprop/rho/Read/ReadVariableOpReadVariableOpRMSprop/rho*
_output_shapes
: *
dtype0
К
gru_1/gru_cell_1/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*(
shared_namegru_1/gru_cell_1/kernel
Г
+gru_1/gru_cell_1/kernel/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/kernel*
_output_shapes

:`*
dtype0
Ю
!gru_1/gru_cell_1/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*2
shared_name#!gru_1/gru_cell_1/recurrent_kernel
Ч
5gru_1/gru_cell_1/recurrent_kernel/Read/ReadVariableOpReadVariableOp!gru_1/gru_cell_1/recurrent_kernel*
_output_shapes

: `*
dtype0
В
gru_1/gru_cell_1/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:`*&
shared_namegru_1/gru_cell_1/bias
{
)gru_1/gru_cell_1/bias/Read/ReadVariableOpReadVariableOpgru_1/gru_cell_1/bias*
_output_shapes
:`*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
Р
RMSprop/dense_1/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *+
shared_nameRMSprop/dense_1/kernel/rms
Й
.RMSprop/dense_1/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_1/kernel/rms*
_output_shapes

: *
dtype0
И
RMSprop/dense_1/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:*)
shared_nameRMSprop/dense_1/bias/rms
Б
,RMSprop/dense_1/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_1/bias/rms*
_output_shapes
:*
dtype0
Ґ
#RMSprop/gru_1/gru_cell_1/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:`*4
shared_name%#RMSprop/gru_1/gru_cell_1/kernel/rms
Ы
7RMSprop/gru_1/gru_cell_1/kernel/rms/Read/ReadVariableOpReadVariableOp#RMSprop/gru_1/gru_cell_1/kernel/rms*
_output_shapes

:`*
dtype0
ґ
-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
: `*>
shared_name/-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rms
ѓ
ARMSprop/gru_1/gru_cell_1/recurrent_kernel/rms/Read/ReadVariableOpReadVariableOp-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rms*
_output_shapes

: `*
dtype0
Ъ
!RMSprop/gru_1/gru_cell_1/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:`*2
shared_name#!RMSprop/gru_1/gru_cell_1/bias/rms
У
5RMSprop/gru_1/gru_cell_1/bias/rms/Read/ReadVariableOpReadVariableOp!RMSprop/gru_1/gru_cell_1/bias/rms*
_output_shapes
:`*
dtype0

NoOpNoOp
∞
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*л
valueбBё B„
њ
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
l
	cell


state_spec
	variables
trainable_variables
regularization_losses
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
v
iter
	decay
learning_rate
momentum
rho	rms;	rms<	rms=	rms>	rms?
#
0
1
2
3
4
#
0
1
2
3
4
 
≠
metrics
non_trainable_variables

layers
 layer_regularization_losses
!layer_metrics
	variables
trainable_variables
regularization_losses
 
~

kernel
recurrent_kernel
bias
"	variables
#trainable_variables
$regularization_losses
%	keras_api
 

0
1
2

0
1
2
 
є
&metrics

'states
(non_trainable_variables

)layers
*layer_regularization_losses
+layer_metrics
	variables
trainable_variables
regularization_losses
ZX
VARIABLE_VALUEdense_1/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUEdense_1/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
≠
,metrics
-non_trainable_variables

.layers
/layer_regularization_losses
0layer_metrics
	variables
trainable_variables
regularization_losses
KI
VARIABLE_VALUERMSprop/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
MK
VARIABLE_VALUERMSprop/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUERMSprop/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUERMSprop/momentum-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUE
IG
VARIABLE_VALUERMSprop/rho(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUEgru_1/gru_cell_1/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUE!gru_1/gru_cell_1/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEgru_1/gru_cell_1/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE

10
 

0
1
 
 

0
1
2

0
1
2
 
≠
2metrics
3non_trainable_variables

4layers
5layer_regularization_losses
6layer_metrics
"	variables
#trainable_variables
$regularization_losses
 
 
 

	0
 
 
 
 
 
 
 
4
	7total
	8count
9	variables
:	keras_api
 
 
 
 
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

70
81

9	variables
ЕВ
VARIABLE_VALUERMSprop/dense_1/kernel/rmsTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
А~
VARIABLE_VALUERMSprop/dense_1/bias/rmsRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
}{
VARIABLE_VALUE#RMSprop/gru_1/gru_cell_1/kernel/rmsDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
ИЕ
VARIABLE_VALUE-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rmsDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
{y
VARIABLE_VALUE!RMSprop/gru_1/gru_cell_1/bias/rmsDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
Ш
serving_default_gru_1_inputPlaceholder*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€*
dtype0*)
shape :€€€€€€€€€€€€€€€€€€
О
StatefulPartitionedCallStatefulPartitionedCallserving_default_gru_1_inputgru_1/gru_cell_1/kernelgru_1/gru_cell_1/bias!gru_1/gru_cell_1/recurrent_kerneldense_1/kerneldense_1/bias*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*,
f'R%
#__inference_signature_wrapper_38566
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
№
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename"dense_1/kernel/Read/ReadVariableOp dense_1/bias/Read/ReadVariableOp RMSprop/iter/Read/ReadVariableOp!RMSprop/decay/Read/ReadVariableOp)RMSprop/learning_rate/Read/ReadVariableOp$RMSprop/momentum/Read/ReadVariableOpRMSprop/rho/Read/ReadVariableOp+gru_1/gru_cell_1/kernel/Read/ReadVariableOp5gru_1/gru_cell_1/recurrent_kernel/Read/ReadVariableOp)gru_1/gru_cell_1/bias/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp.RMSprop/dense_1/kernel/rms/Read/ReadVariableOp,RMSprop/dense_1/bias/rms/Read/ReadVariableOp7RMSprop/gru_1/gru_cell_1/kernel/rms/Read/ReadVariableOpARMSprop/gru_1/gru_cell_1/recurrent_kernel/rms/Read/ReadVariableOp5RMSprop/gru_1/gru_cell_1/bias/rms/Read/ReadVariableOpConst*
Tin
2	*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 **
config_proto

CPU

GPU 2J 8*'
f"R 
__inference__traced_save_41001
Г
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense_1/kerneldense_1/biasRMSprop/iterRMSprop/decayRMSprop/learning_rateRMSprop/momentumRMSprop/rhogru_1/gru_cell_1/kernel!gru_1/gru_cell_1/recurrent_kernelgru_1/gru_cell_1/biastotalcountRMSprop/dense_1/kernel/rmsRMSprop/dense_1/bias/rms#RMSprop/gru_1/gru_cell_1/kernel/rms-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rms!RMSprop/gru_1/gru_cell_1/bias/rms*
Tin
2*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 **
config_proto

CPU

GPU 2J 8**
f%R#
!__inference__traced_restore_41064Л«$
м4
П
__inference__traced_save_41001
file_prefix-
)savev2_dense_1_kernel_read_readvariableop+
'savev2_dense_1_bias_read_readvariableop+
'savev2_rmsprop_iter_read_readvariableop	,
(savev2_rmsprop_decay_read_readvariableop4
0savev2_rmsprop_learning_rate_read_readvariableop/
+savev2_rmsprop_momentum_read_readvariableop*
&savev2_rmsprop_rho_read_readvariableop6
2savev2_gru_1_gru_cell_1_kernel_read_readvariableop@
<savev2_gru_1_gru_cell_1_recurrent_kernel_read_readvariableop4
0savev2_gru_1_gru_cell_1_bias_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop9
5savev2_rmsprop_dense_1_kernel_rms_read_readvariableop7
3savev2_rmsprop_dense_1_bias_rms_read_readvariableopB
>savev2_rmsprop_gru_1_gru_cell_1_kernel_rms_read_readvariableopL
Hsavev2_rmsprop_gru_1_gru_cell_1_recurrent_kernel_rms_read_readvariableop@
<savev2_rmsprop_gru_1_gru_cell_1_bias_rms_read_readvariableop
savev2_1_const

identity_1ИҐMergeV2CheckpointsҐSaveV2ҐSaveV2_1П
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
ConstН
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*<
value3B1 B+_temp_f19a2fc3ffcb4285a666d3710b9ada0a/part2	
Const_1Л
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard¶
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilenameµ
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*«
valueљBЇB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE2
SaveV2/tensor_names™
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slicesЖ
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0)savev2_dense_1_kernel_read_readvariableop'savev2_dense_1_bias_read_readvariableop'savev2_rmsprop_iter_read_readvariableop(savev2_rmsprop_decay_read_readvariableop0savev2_rmsprop_learning_rate_read_readvariableop+savev2_rmsprop_momentum_read_readvariableop&savev2_rmsprop_rho_read_readvariableop2savev2_gru_1_gru_cell_1_kernel_read_readvariableop<savev2_gru_1_gru_cell_1_recurrent_kernel_read_readvariableop0savev2_gru_1_gru_cell_1_bias_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop5savev2_rmsprop_dense_1_kernel_rms_read_readvariableop3savev2_rmsprop_dense_1_bias_rms_read_readvariableop>savev2_rmsprop_gru_1_gru_cell_1_kernel_rms_read_readvariableopHsavev2_rmsprop_gru_1_gru_cell_1_recurrent_kernel_rms_read_readvariableop<savev2_rmsprop_gru_1_gru_cell_1_bias_rms_read_readvariableop"/device:CPU:0*
_output_shapes
 *
dtypes
2	2
SaveV2Г
ShardedFilename_1/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B :2
ShardedFilename_1/shardђ
ShardedFilename_1ShardedFilenameStringJoin:output:0 ShardedFilename_1/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename_1Ґ
SaveV2_1/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*1
value(B&B_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2_1/tensor_namesО
SaveV2_1/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*
valueB
B 2
SaveV2_1/shape_and_slicesѕ
SaveV2_1SaveV2ShardedFilename_1:filename:0SaveV2_1/tensor_names:output:0"SaveV2_1/shape_and_slices:output:0savev2_1_const^SaveV2"/device:CPU:0*
_output_shapes
 *
dtypes
22

SaveV2_1г
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0ShardedFilename_1:filename:0^SaveV2	^SaveV2_1"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixesђ
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix	^SaveV2_1"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

IdentityБ

Identity_1IdentityIdentity:output:0^MergeV2Checkpoints^SaveV2	^SaveV2_1*
T0*
_output_shapes
: 2

Identity_1"!

identity_1Identity_1:output:0*y
_input_shapesh
f: : :: : : : : :`: `:`: : : ::`: `:`: 2(
MergeV2CheckpointsMergeV2Checkpoints2
SaveV2SaveV22
SaveV2_1SaveV2_1:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

: : 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:`:$	 

_output_shapes

: `: 


_output_shapes
:`:

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

: : 

_output_shapes
::$ 

_output_shapes

:`:$ 

_output_shapes

: `: 

_output_shapes
:`:

_output_shapes
: 
≥
г
while_cond_37552
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_37552___redundant_placeholder0-
)while_cond_37552___redundant_placeholder1-
)while_cond_37552___redundant_placeholder2-
)while_cond_37552___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
§O
„	
!__inference__traced_restore_41064
file_prefix#
assignvariableop_dense_1_kernel#
assignvariableop_1_dense_1_bias#
assignvariableop_2_rmsprop_iter$
 assignvariableop_3_rmsprop_decay,
(assignvariableop_4_rmsprop_learning_rate'
#assignvariableop_5_rmsprop_momentum"
assignvariableop_6_rmsprop_rho.
*assignvariableop_7_gru_1_gru_cell_1_kernel8
4assignvariableop_8_gru_1_gru_cell_1_recurrent_kernel,
(assignvariableop_9_gru_1_gru_cell_1_bias
assignvariableop_10_total
assignvariableop_11_count2
.assignvariableop_12_rmsprop_dense_1_kernel_rms0
,assignvariableop_13_rmsprop_dense_1_bias_rms;
7assignvariableop_14_rmsprop_gru_1_gru_cell_1_kernel_rmsE
Aassignvariableop_15_rmsprop_gru_1_gru_cell_1_recurrent_kernel_rms9
5assignvariableop_16_rmsprop_gru_1_gru_cell_1_bias_rms
identity_18ИҐAssignVariableOpҐAssignVariableOp_1ҐAssignVariableOp_10ҐAssignVariableOp_11ҐAssignVariableOp_12ҐAssignVariableOp_13ҐAssignVariableOp_14ҐAssignVariableOp_15ҐAssignVariableOp_16ҐAssignVariableOp_2ҐAssignVariableOp_3ҐAssignVariableOp_4ҐAssignVariableOp_5ҐAssignVariableOp_6ҐAssignVariableOp_7ҐAssignVariableOp_8ҐAssignVariableOp_9Ґ	RestoreV2ҐRestoreV2_1ї
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*«
valueљBЇB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE2
RestoreV2/tensor_names∞
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slicesА
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*X
_output_shapesF
D:::::::::::::::::*
dtypes
2	2
	RestoreV2X
IdentityIdentityRestoreV2:tensors:0*
T0*
_output_shapes
:2

IdentityП
AssignVariableOpAssignVariableOpassignvariableop_dense_1_kernelIdentity:output:0*
_output_shapes
 *
dtype02
AssignVariableOp\

Identity_1IdentityRestoreV2:tensors:1*
T0*
_output_shapes
:2

Identity_1Х
AssignVariableOp_1AssignVariableOpassignvariableop_1_dense_1_biasIdentity_1:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_1\

Identity_2IdentityRestoreV2:tensors:2*
T0	*
_output_shapes
:2

Identity_2Х
AssignVariableOp_2AssignVariableOpassignvariableop_2_rmsprop_iterIdentity_2:output:0*
_output_shapes
 *
dtype0	2
AssignVariableOp_2\

Identity_3IdentityRestoreV2:tensors:3*
T0*
_output_shapes
:2

Identity_3Ц
AssignVariableOp_3AssignVariableOp assignvariableop_3_rmsprop_decayIdentity_3:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_3\

Identity_4IdentityRestoreV2:tensors:4*
T0*
_output_shapes
:2

Identity_4Ю
AssignVariableOp_4AssignVariableOp(assignvariableop_4_rmsprop_learning_rateIdentity_4:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_4\

Identity_5IdentityRestoreV2:tensors:5*
T0*
_output_shapes
:2

Identity_5Щ
AssignVariableOp_5AssignVariableOp#assignvariableop_5_rmsprop_momentumIdentity_5:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_5\

Identity_6IdentityRestoreV2:tensors:6*
T0*
_output_shapes
:2

Identity_6Ф
AssignVariableOp_6AssignVariableOpassignvariableop_6_rmsprop_rhoIdentity_6:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_6\

Identity_7IdentityRestoreV2:tensors:7*
T0*
_output_shapes
:2

Identity_7†
AssignVariableOp_7AssignVariableOp*assignvariableop_7_gru_1_gru_cell_1_kernelIdentity_7:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_7\

Identity_8IdentityRestoreV2:tensors:8*
T0*
_output_shapes
:2

Identity_8™
AssignVariableOp_8AssignVariableOp4assignvariableop_8_gru_1_gru_cell_1_recurrent_kernelIdentity_8:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_8\

Identity_9IdentityRestoreV2:tensors:9*
T0*
_output_shapes
:2

Identity_9Ю
AssignVariableOp_9AssignVariableOp(assignvariableop_9_gru_1_gru_cell_1_biasIdentity_9:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_9_
Identity_10IdentityRestoreV2:tensors:10*
T0*
_output_shapes
:2
Identity_10Т
AssignVariableOp_10AssignVariableOpassignvariableop_10_totalIdentity_10:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_10_
Identity_11IdentityRestoreV2:tensors:11*
T0*
_output_shapes
:2
Identity_11Т
AssignVariableOp_11AssignVariableOpassignvariableop_11_countIdentity_11:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_11_
Identity_12IdentityRestoreV2:tensors:12*
T0*
_output_shapes
:2
Identity_12І
AssignVariableOp_12AssignVariableOp.assignvariableop_12_rmsprop_dense_1_kernel_rmsIdentity_12:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_12_
Identity_13IdentityRestoreV2:tensors:13*
T0*
_output_shapes
:2
Identity_13•
AssignVariableOp_13AssignVariableOp,assignvariableop_13_rmsprop_dense_1_bias_rmsIdentity_13:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_13_
Identity_14IdentityRestoreV2:tensors:14*
T0*
_output_shapes
:2
Identity_14∞
AssignVariableOp_14AssignVariableOp7assignvariableop_14_rmsprop_gru_1_gru_cell_1_kernel_rmsIdentity_14:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_14_
Identity_15IdentityRestoreV2:tensors:15*
T0*
_output_shapes
:2
Identity_15Ї
AssignVariableOp_15AssignVariableOpAassignvariableop_15_rmsprop_gru_1_gru_cell_1_recurrent_kernel_rmsIdentity_15:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_15_
Identity_16IdentityRestoreV2:tensors:16*
T0*
_output_shapes
:2
Identity_16Ѓ
AssignVariableOp_16AssignVariableOp5assignvariableop_16_rmsprop_gru_1_gru_cell_1_bias_rmsIdentity_16:output:0*
_output_shapes
 *
dtype02
AssignVariableOp_16®
RestoreV2_1/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*1
value(B&B_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2_1/tensor_namesФ
RestoreV2_1/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*
valueB
B 2
RestoreV2_1/shape_and_slicesƒ
RestoreV2_1	RestoreV2file_prefix!RestoreV2_1/tensor_names:output:0%RestoreV2_1/shape_and_slices:output:0
^RestoreV2"/device:CPU:0*
_output_shapes
:*
dtypes
22
RestoreV2_19
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp‘
Identity_17Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_17б
Identity_18IdentityIdentity_17:output:0^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9
^RestoreV2^RestoreV2_1*
T0*
_output_shapes
: 2
Identity_18"#
identity_18Identity_18:output:0*Y
_input_shapesH
F: :::::::::::::::::2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162(
AssignVariableOp_2AssignVariableOp_22(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_92
	RestoreV2	RestoreV22
RestoreV2_1RestoreV2_1:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: :


_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
А=
ќ
@__inference_gru_1_layer_call_and_return_conditional_losses_37735

inputs
gru_cell_1_37659
gru_cell_1_37661
gru_cell_1_37663
identityИҐ"gru_cell_1/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2ƒ
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_1_37659gru_cell_1_37661gru_cell_1_37663*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_372942$
"gru_cell_1/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterя
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_1_37659gru_cell_1_37661gru_cell_1_37663*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_37671*
condR
while_cond_37670*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeЩ
IdentityIdentitystrided_slice_3:output:0#^gru_cell_1/StatefulPartitionedCall^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Хх
г
@__inference_gru_1_layer_call_and_return_conditional_losses_38124

inputs&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2њЖў21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeъ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2…ЯЎ23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2Ѓлн23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1z
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ЃИ£23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeъ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ЉЊе23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2шХЪ23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1С
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЧ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ч
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_37925*
condR
while_cond_37924*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
г
while_cond_37670
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_37670___redundant_placeholder0-
)while_cond_37670___redundant_placeholder1-
)while_cond_37670___redundant_placeholder2-
)while_cond_37670___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
й
У
gru_1_while_cond_39078
gru_1_while_loop_counter"
gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_gru_1_strided_slice_13
/gru_1_while_cond_39078___redundant_placeholder03
/gru_1_while_cond_39078___redundant_placeholder13
/gru_1_while_cond_39078___redundant_placeholder23
/gru_1_while_cond_39078___redundant_placeholder3
identity
^
LessLessplaceholderless_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Т

ђ
*__inference_gru_cell_1_layer_call_fn_40923

inputs
states_0
unknown
	unknown_0
	unknown_1
identity

identity_1ИҐStatefulPartitionedCall€
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_372942
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityТ

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ :::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
З
™
B__inference_dense_1_layer_call_and_return_conditional_losses_40636

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identityИН
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
MatMulМ
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOpБ
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2	
BiasAddd
IdentityIdentityBiasAdd:output:0*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*.
_input_shapes
:€€€€€€€€€ :::O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
а
≥
,__inference_sequential_1_layer_call_fn_38541
gru_1_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identityИҐStatefulPartitionedCallБ
StatefulPartitionedCallStatefulPartitionedCallgru_1_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*P
fKRI
G__inference_sequential_1_layer_call_and_return_conditional_losses_385282
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::22
StatefulPartitionedCallStatefulPartitionedCall:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
З
™
B__inference_dense_1_layer_call_and_return_conditional_losses_38445

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identityИН
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
MatMulМ
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOpБ
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2	
BiasAddd
IdentityIdentityBiasAdd:output:0*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*.
_input_shapes
:€€€€€€€€€ :::O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
‘Т
Ю
while_body_39773
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1§
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul®
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1®
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
бы
З
 __inference__wrapped_model_37012
gru_1_input9
5sequential_1_gru_1_gru_cell_1_readvariableop_resource;
7sequential_1_gru_1_gru_cell_1_readvariableop_3_resource;
7sequential_1_gru_1_gru_cell_1_readvariableop_6_resource7
3sequential_1_dense_1_matmul_readvariableop_resource8
4sequential_1_dense_1_biasadd_readvariableop_resource
identityИҐsequential_1/gru_1/whileo
sequential_1/gru_1/ShapeShapegru_1_input*
T0*
_output_shapes
:2
sequential_1/gru_1/ShapeЪ
&sequential_1/gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&sequential_1/gru_1/strided_slice/stackЮ
(sequential_1/gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential_1/gru_1/strided_slice/stack_1Ю
(sequential_1/gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(sequential_1/gru_1/strided_slice/stack_2‘
 sequential_1/gru_1/strided_sliceStridedSlice!sequential_1/gru_1/Shape:output:0/sequential_1/gru_1/strided_slice/stack:output:01sequential_1/gru_1/strided_slice/stack_1:output:01sequential_1/gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2"
 sequential_1/gru_1/strided_sliceВ
sequential_1/gru_1/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2 
sequential_1/gru_1/zeros/mul/yЄ
sequential_1/gru_1/zeros/mulMul)sequential_1/gru_1/strided_slice:output:0'sequential_1/gru_1/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
sequential_1/gru_1/zeros/mulЕ
sequential_1/gru_1/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2!
sequential_1/gru_1/zeros/Less/y≥
sequential_1/gru_1/zeros/LessLess sequential_1/gru_1/zeros/mul:z:0(sequential_1/gru_1/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
sequential_1/gru_1/zeros/LessИ
!sequential_1/gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2#
!sequential_1/gru_1/zeros/packed/1ѕ
sequential_1/gru_1/zeros/packedPack)sequential_1/gru_1/strided_slice:output:0*sequential_1/gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2!
sequential_1/gru_1/zeros/packedЕ
sequential_1/gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
sequential_1/gru_1/zeros/ConstЅ
sequential_1/gru_1/zerosFill(sequential_1/gru_1/zeros/packed:output:0'sequential_1/gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sequential_1/gru_1/zerosЫ
!sequential_1/gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2#
!sequential_1/gru_1/transpose/permЅ
sequential_1/gru_1/transpose	Transposegru_1_input*sequential_1/gru_1/transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
sequential_1/gru_1/transposeИ
sequential_1/gru_1/Shape_1Shape sequential_1/gru_1/transpose:y:0*
T0*
_output_shapes
:2
sequential_1/gru_1/Shape_1Ю
(sequential_1/gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_1/gru_1/strided_slice_1/stackҐ
*sequential_1/gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_1/gru_1/strided_slice_1/stack_1Ґ
*sequential_1/gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_1/gru_1/strided_slice_1/stack_2а
"sequential_1/gru_1/strided_slice_1StridedSlice#sequential_1/gru_1/Shape_1:output:01sequential_1/gru_1/strided_slice_1/stack:output:03sequential_1/gru_1/strided_slice_1/stack_1:output:03sequential_1/gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2$
"sequential_1/gru_1/strided_slice_1Ђ
.sequential_1/gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€20
.sequential_1/gru_1/TensorArrayV2/element_shapeю
 sequential_1/gru_1/TensorArrayV2TensorListReserve7sequential_1/gru_1/TensorArrayV2/element_shape:output:0+sequential_1/gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02"
 sequential_1/gru_1/TensorArrayV2е
Hsequential_1/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2J
Hsequential_1/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeƒ
:sequential_1/gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor sequential_1/gru_1/transpose:y:0Qsequential_1/gru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02<
:sequential_1/gru_1/TensorArrayUnstack/TensorListFromTensorЮ
(sequential_1/gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_1/gru_1/strided_slice_2/stackҐ
*sequential_1/gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_1/gru_1/strided_slice_2/stack_1Ґ
*sequential_1/gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_1/gru_1/strided_slice_2/stack_2о
"sequential_1/gru_1/strided_slice_2StridedSlice sequential_1/gru_1/transpose:y:01sequential_1/gru_1/strided_slice_2/stack:output:03sequential_1/gru_1/strided_slice_2/stack_1:output:03sequential_1/gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2$
"sequential_1/gru_1/strided_slice_2є
-sequential_1/gru_1/gru_cell_1/ones_like/ShapeShape+sequential_1/gru_1/strided_slice_2:output:0*
T0*
_output_shapes
:2/
-sequential_1/gru_1/gru_cell_1/ones_like/Shape£
-sequential_1/gru_1/gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2/
-sequential_1/gru_1/gru_cell_1/ones_like/Constь
'sequential_1/gru_1/gru_cell_1/ones_likeFill6sequential_1/gru_1/gru_cell_1/ones_like/Shape:output:06sequential_1/gru_1/gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2)
'sequential_1/gru_1/gru_cell_1/ones_like≥
/sequential_1/gru_1/gru_cell_1/ones_like_1/ShapeShape!sequential_1/gru_1/zeros:output:0*
T0*
_output_shapes
:21
/sequential_1/gru_1/gru_cell_1/ones_like_1/ShapeІ
/sequential_1/gru_1/gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?21
/sequential_1/gru_1/gru_cell_1/ones_like_1/ConstД
)sequential_1/gru_1/gru_cell_1/ones_like_1Fill8sequential_1/gru_1/gru_cell_1/ones_like_1/Shape:output:08sequential_1/gru_1/gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2+
)sequential_1/gru_1/gru_cell_1/ones_like_1ё
!sequential_1/gru_1/gru_cell_1/mulMul+sequential_1/gru_1/strided_slice_2:output:00sequential_1/gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!sequential_1/gru_1/gru_cell_1/mulв
#sequential_1/gru_1/gru_cell_1/mul_1Mul+sequential_1/gru_1/strided_slice_2:output:00sequential_1/gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2%
#sequential_1/gru_1/gru_cell_1/mul_1в
#sequential_1/gru_1/gru_cell_1/mul_2Mul+sequential_1/gru_1/strided_slice_2:output:00sequential_1/gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2%
#sequential_1/gru_1/gru_cell_1/mul_2“
,sequential_1/gru_1/gru_cell_1/ReadVariableOpReadVariableOp5sequential_1_gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02.
,sequential_1/gru_1/gru_cell_1/ReadVariableOpЈ
1sequential_1/gru_1/gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        23
1sequential_1/gru_1/gru_cell_1/strided_slice/stackї
3sequential_1/gru_1/gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        25
3sequential_1/gru_1/gru_cell_1/strided_slice/stack_1ї
3sequential_1/gru_1/gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      25
3sequential_1/gru_1/gru_cell_1/strided_slice/stack_2∞
+sequential_1/gru_1/gru_cell_1/strided_sliceStridedSlice4sequential_1/gru_1/gru_cell_1/ReadVariableOp:value:0:sequential_1/gru_1/gru_cell_1/strided_slice/stack:output:0<sequential_1/gru_1/gru_cell_1/strided_slice/stack_1:output:0<sequential_1/gru_1/gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2-
+sequential_1/gru_1/gru_cell_1/strided_sliceе
$sequential_1/gru_1/gru_cell_1/MatMulMatMul%sequential_1/gru_1/gru_cell_1/mul:z:04sequential_1/gru_1/gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2&
$sequential_1/gru_1/gru_cell_1/MatMul÷
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_1ReadVariableOp5sequential_1_gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_1ї
3sequential_1/gru_1/gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        25
3sequential_1/gru_1/gru_cell_1/strided_slice_1/stackњ
5sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   27
5sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_1њ
5sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      27
5sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_2Љ
-sequential_1/gru_1/gru_cell_1/strided_slice_1StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_1:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_1/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_1н
&sequential_1/gru_1/gru_cell_1/MatMul_1MatMul'sequential_1/gru_1/gru_cell_1/mul_1:z:06sequential_1/gru_1/gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential_1/gru_1/gru_cell_1/MatMul_1÷
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_2ReadVariableOp5sequential_1_gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_2ї
3sequential_1/gru_1/gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   25
3sequential_1/gru_1/gru_cell_1/strided_slice_2/stackњ
5sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        27
5sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_1њ
5sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      27
5sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_2Љ
-sequential_1/gru_1/gru_cell_1/strided_slice_2StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_2:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_2/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_2н
&sequential_1/gru_1/gru_cell_1/MatMul_2MatMul'sequential_1/gru_1/gru_cell_1/mul_2:z:06sequential_1/gru_1/gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential_1/gru_1/gru_cell_1/MatMul_2‘
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_3ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_3і
3sequential_1/gru_1/gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 25
3sequential_1/gru_1/gru_cell_1/strided_slice_3/stackЄ
5sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 27
5sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_1Є
5sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:27
5sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_2®
-sequential_1/gru_1/gru_cell_1/strided_slice_3StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_3:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_3/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_3у
%sequential_1/gru_1/gru_cell_1/BiasAddBiasAdd.sequential_1/gru_1/gru_cell_1/MatMul:product:06sequential_1/gru_1/gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2'
%sequential_1/gru_1/gru_cell_1/BiasAdd‘
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_4ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_4і
3sequential_1/gru_1/gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 25
3sequential_1/gru_1/gru_cell_1/strided_slice_4/stackЄ
5sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@27
5sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_1Є
5sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:27
5sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_2Ц
-sequential_1/gru_1/gru_cell_1/strided_slice_4StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_4:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_4/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2/
-sequential_1/gru_1/gru_cell_1/strided_slice_4щ
'sequential_1/gru_1/gru_cell_1/BiasAdd_1BiasAdd0sequential_1/gru_1/gru_cell_1/MatMul_1:product:06sequential_1/gru_1/gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential_1/gru_1/gru_cell_1/BiasAdd_1‘
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_5ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_5і
3sequential_1/gru_1/gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@25
3sequential_1/gru_1/gru_cell_1/strided_slice_5/stackЄ
5sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 27
5sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_1Є
5sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:27
5sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_2¶
-sequential_1/gru_1/gru_cell_1/strided_slice_5StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_5:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_5/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_5щ
'sequential_1/gru_1/gru_cell_1/BiasAdd_2BiasAdd0sequential_1/gru_1/gru_cell_1/MatMul_2:product:06sequential_1/gru_1/gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'sequential_1/gru_1/gru_cell_1/BiasAdd_2Џ
#sequential_1/gru_1/gru_cell_1/mul_3Mul!sequential_1/gru_1/zeros:output:02sequential_1/gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/mul_3Џ
#sequential_1/gru_1/gru_cell_1/mul_4Mul!sequential_1/gru_1/zeros:output:02sequential_1/gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/mul_4Џ
#sequential_1/gru_1/gru_cell_1/mul_5Mul!sequential_1/gru_1/zeros:output:02sequential_1/gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/mul_5Ў
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_6ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_6ї
3sequential_1/gru_1/gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        25
3sequential_1/gru_1/gru_cell_1/strided_slice_6/stackњ
5sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        27
5sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_1њ
5sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      27
5sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_2Љ
-sequential_1/gru_1/gru_cell_1/strided_slice_6StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_6:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_6/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_6н
&sequential_1/gru_1/gru_cell_1/MatMul_3MatMul'sequential_1/gru_1/gru_cell_1/mul_3:z:06sequential_1/gru_1/gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential_1/gru_1/gru_cell_1/MatMul_3Ў
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_7ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_7ї
3sequential_1/gru_1/gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        25
3sequential_1/gru_1/gru_cell_1/strided_slice_7/stackњ
5sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   27
5sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_1њ
5sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      27
5sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_2Љ
-sequential_1/gru_1/gru_cell_1/strided_slice_7StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_7:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_7/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_7н
&sequential_1/gru_1/gru_cell_1/MatMul_4MatMul'sequential_1/gru_1/gru_cell_1/mul_4:z:06sequential_1/gru_1/gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential_1/gru_1/gru_cell_1/MatMul_4г
!sequential_1/gru_1/gru_cell_1/addAddV2.sequential_1/gru_1/gru_cell_1/BiasAdd:output:00sequential_1/gru_1/gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential_1/gru_1/gru_cell_1/addП
#sequential_1/gru_1/gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#sequential_1/gru_1/gru_cell_1/ConstУ
%sequential_1/gru_1/gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2'
%sequential_1/gru_1/gru_cell_1/Const_1Ў
#sequential_1/gru_1/gru_cell_1/Mul_6Mul%sequential_1/gru_1/gru_cell_1/add:z:0,sequential_1/gru_1/gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/Mul_6№
#sequential_1/gru_1/gru_cell_1/Add_1Add'sequential_1/gru_1/gru_cell_1/Mul_6:z:0.sequential_1/gru_1/gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/Add_1≥
5sequential_1/gru_1/gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?27
5sequential_1/gru_1/gru_cell_1/clip_by_value/Minimum/yР
3sequential_1/gru_1/gru_cell_1/clip_by_value/MinimumMinimum'sequential_1/gru_1/gru_cell_1/Add_1:z:0>sequential_1/gru_1/gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 25
3sequential_1/gru_1/gru_cell_1/clip_by_value/Minimum£
-sequential_1/gru_1/gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2/
-sequential_1/gru_1/gru_cell_1/clip_by_value/yИ
+sequential_1/gru_1/gru_cell_1/clip_by_valueMaximum7sequential_1/gru_1/gru_cell_1/clip_by_value/Minimum:z:06sequential_1/gru_1/gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2-
+sequential_1/gru_1/gru_cell_1/clip_by_valueй
#sequential_1/gru_1/gru_cell_1/add_2AddV20sequential_1/gru_1/gru_cell_1/BiasAdd_1:output:00sequential_1/gru_1/gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/add_2У
%sequential_1/gru_1/gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2'
%sequential_1/gru_1/gru_cell_1/Const_2У
%sequential_1/gru_1/gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2'
%sequential_1/gru_1/gru_cell_1/Const_3№
#sequential_1/gru_1/gru_cell_1/Mul_7Mul'sequential_1/gru_1/gru_cell_1/add_2:z:0.sequential_1/gru_1/gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/Mul_7№
#sequential_1/gru_1/gru_cell_1/Add_3Add'sequential_1/gru_1/gru_cell_1/Mul_7:z:0.sequential_1/gru_1/gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/Add_3Ј
7sequential_1/gru_1/gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?29
7sequential_1/gru_1/gru_cell_1/clip_by_value_1/Minimum/yЦ
5sequential_1/gru_1/gru_cell_1/clip_by_value_1/MinimumMinimum'sequential_1/gru_1/gru_cell_1/Add_3:z:0@sequential_1/gru_1/gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 27
5sequential_1/gru_1/gru_cell_1/clip_by_value_1/MinimumІ
/sequential_1/gru_1/gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    21
/sequential_1/gru_1/gru_cell_1/clip_by_value_1/yР
-sequential_1/gru_1/gru_cell_1/clip_by_value_1Maximum9sequential_1/gru_1/gru_cell_1/clip_by_value_1/Minimum:z:08sequential_1/gru_1/gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2/
-sequential_1/gru_1/gru_cell_1/clip_by_value_1я
#sequential_1/gru_1/gru_cell_1/mul_8Mul1sequential_1/gru_1/gru_cell_1/clip_by_value_1:z:0'sequential_1/gru_1/gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/mul_8Ў
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_8ReadVariableOp7sequential_1_gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype020
.sequential_1/gru_1/gru_cell_1/ReadVariableOp_8ї
3sequential_1/gru_1/gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   25
3sequential_1/gru_1/gru_cell_1/strided_slice_8/stackњ
5sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        27
5sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_1њ
5sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      27
5sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_2Љ
-sequential_1/gru_1/gru_cell_1/strided_slice_8StridedSlice6sequential_1/gru_1/gru_cell_1/ReadVariableOp_8:value:0<sequential_1/gru_1/gru_cell_1/strided_slice_8/stack:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_1:output:0>sequential_1/gru_1/gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2/
-sequential_1/gru_1/gru_cell_1/strided_slice_8н
&sequential_1/gru_1/gru_cell_1/MatMul_5MatMul'sequential_1/gru_1/gru_cell_1/mul_8:z:06sequential_1/gru_1/gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&sequential_1/gru_1/gru_cell_1/MatMul_5й
#sequential_1/gru_1/gru_cell_1/add_4AddV20sequential_1/gru_1/gru_cell_1/BiasAdd_2:output:00sequential_1/gru_1/gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/add_4Ђ
"sequential_1/gru_1/gru_cell_1/TanhTanh'sequential_1/gru_1/gru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"sequential_1/gru_1/gru_cell_1/Tanh„
#sequential_1/gru_1/gru_cell_1/mul_9Mul/sequential_1/gru_1/gru_cell_1/clip_by_value:z:0!sequential_1/gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/mul_9П
#sequential_1/gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2%
#sequential_1/gru_1/gru_cell_1/sub/xё
!sequential_1/gru_1/gru_cell_1/subSub,sequential_1/gru_1/gru_cell_1/sub/x:output:0/sequential_1/gru_1/gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!sequential_1/gru_1/gru_cell_1/sub‘
$sequential_1/gru_1/gru_cell_1/mul_10Mul%sequential_1/gru_1/gru_cell_1/sub:z:0&sequential_1/gru_1/gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2&
$sequential_1/gru_1/gru_cell_1/mul_10Ў
#sequential_1/gru_1/gru_cell_1/add_5AddV2'sequential_1/gru_1/gru_cell_1/mul_9:z:0(sequential_1/gru_1/gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2%
#sequential_1/gru_1/gru_cell_1/add_5µ
0sequential_1/gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0sequential_1/gru_1/TensorArrayV2_1/element_shapeД
"sequential_1/gru_1/TensorArrayV2_1TensorListReserve9sequential_1/gru_1/TensorArrayV2_1/element_shape:output:0+sequential_1/gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02$
"sequential_1/gru_1/TensorArrayV2_1t
sequential_1/gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_1/gru_1/time•
+sequential_1/gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2-
+sequential_1/gru_1/while/maximum_iterationsР
%sequential_1/gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2'
%sequential_1/gru_1/while/loop_counter£
sequential_1/gru_1/whileWhile.sequential_1/gru_1/while/loop_counter:output:04sequential_1/gru_1/while/maximum_iterations:output:0 sequential_1/gru_1/time:output:0+sequential_1/gru_1/TensorArrayV2_1:handle:0!sequential_1/gru_1/zeros:output:0+sequential_1/gru_1/strided_slice_1:output:0Jsequential_1/gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:05sequential_1_gru_1_gru_cell_1_readvariableop_resource7sequential_1_gru_1_gru_cell_1_readvariableop_3_resource7sequential_1_gru_1_gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*/
body'R%
#sequential_1_gru_1_while_body_36855*/
cond'R%
#sequential_1_gru_1_while_cond_36854*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
sequential_1/gru_1/whileџ
Csequential_1/gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2E
Csequential_1/gru_1/TensorArrayV2Stack/TensorListStack/element_shapeљ
5sequential_1/gru_1/TensorArrayV2Stack/TensorListStackTensorListStack!sequential_1/gru_1/while:output:3Lsequential_1/gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype027
5sequential_1/gru_1/TensorArrayV2Stack/TensorListStackІ
(sequential_1/gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2*
(sequential_1/gru_1/strided_slice_3/stackҐ
*sequential_1/gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_1/gru_1/strided_slice_3/stack_1Ґ
*sequential_1/gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_1/gru_1/strided_slice_3/stack_2М
"sequential_1/gru_1/strided_slice_3StridedSlice>sequential_1/gru_1/TensorArrayV2Stack/TensorListStack:tensor:01sequential_1/gru_1/strided_slice_3/stack:output:03sequential_1/gru_1/strided_slice_3/stack_1:output:03sequential_1/gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2$
"sequential_1/gru_1/strided_slice_3Я
#sequential_1/gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2%
#sequential_1/gru_1/transpose_1/permъ
sequential_1/gru_1/transpose_1	Transpose>sequential_1/gru_1/TensorArrayV2Stack/TensorListStack:tensor:0,sequential_1/gru_1/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2 
sequential_1/gru_1/transpose_1М
sequential_1/gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_1/gru_1/runtimeћ
*sequential_1/dense_1/MatMul/ReadVariableOpReadVariableOp3sequential_1_dense_1_matmul_readvariableop_resource*
_output_shapes

: *
dtype02,
*sequential_1/dense_1/MatMul/ReadVariableOp„
sequential_1/dense_1/MatMulMatMul+sequential_1/gru_1/strided_slice_3:output:02sequential_1/dense_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
sequential_1/dense_1/MatMulЋ
+sequential_1/dense_1/BiasAdd/ReadVariableOpReadVariableOp4sequential_1_dense_1_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02-
+sequential_1/dense_1/BiasAdd/ReadVariableOp’
sequential_1/dense_1/BiasAddBiasAdd%sequential_1/dense_1/MatMul:product:03sequential_1/dense_1/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
sequential_1/dense_1/BiasAddФ
IdentityIdentity%sequential_1/dense_1/BiasAdd:output:0^sequential_1/gru_1/while*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::24
sequential_1/gru_1/whilesequential_1/gru_1/while:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
г
while_cond_40123
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_40123___redundant_placeholder0-
)while_cond_40123___redundant_placeholder1-
)while_cond_40123___redundant_placeholder2-
)while_cond_40123___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
≥
г
while_cond_39772
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_39772___redundant_placeholder0-
)while_cond_39772___redundant_placeholder1-
)while_cond_39772___redundant_placeholder2-
)while_cond_39772___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
ЊЌ
џ
G__inference_sequential_1_layer_call_and_return_conditional_losses_39236

inputs,
(gru_1_gru_cell_1_readvariableop_resource.
*gru_1_gru_cell_1_readvariableop_3_resource.
*gru_1_gru_cell_1_readvariableop_6_resource*
&dense_1_matmul_readvariableop_resource+
'dense_1_biasadd_readvariableop_resource
identityИҐgru_1/whileP
gru_1/ShapeShapeinputs*
T0*
_output_shapes
:2
gru_1/ShapeА
gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice/stackД
gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_1Д
gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_2Ж
gru_1/strided_sliceStridedSlicegru_1/Shape:output:0"gru_1/strided_slice/stack:output:0$gru_1/strided_slice/stack_1:output:0$gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_sliceh
gru_1/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/mul/yД
gru_1/zeros/mulMulgru_1/strided_slice:output:0gru_1/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
gru_1/zeros/mulk
gru_1/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
gru_1/zeros/Less/y
gru_1/zeros/LessLessgru_1/zeros/mul:z:0gru_1/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
gru_1/zeros/Lessn
gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/packed/1Ы
gru_1/zeros/packedPackgru_1/strided_slice:output:0gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru_1/zeros/packedk
gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/zeros/ConstН
gru_1/zerosFillgru_1/zeros/packed:output:0gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/zerosБ
gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose/permХ
gru_1/transpose	Transposeinputsgru_1/transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
gru_1/transposea
gru_1/Shape_1Shapegru_1/transpose:y:0*
T0*
_output_shapes
:2
gru_1/Shape_1Д
gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_1/stackИ
gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_1И
gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_2Т
gru_1/strided_slice_1StridedSlicegru_1/Shape_1:output:0$gru_1/strided_slice_1/stack:output:0&gru_1/strided_slice_1/stack_1:output:0&gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slice_1С
!gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2#
!gru_1/TensorArrayV2/element_shape 
gru_1/TensorArrayV2TensorListReserve*gru_1/TensorArrayV2/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2Ћ
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2=
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeР
-gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru_1/transpose:y:0Dgru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02/
-gru_1/TensorArrayUnstack/TensorListFromTensorД
gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_2/stackИ
gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_1И
gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_2†
gru_1/strided_slice_2StridedSlicegru_1/transpose:y:0$gru_1/strided_slice_2/stack:output:0&gru_1/strided_slice_2/stack_1:output:0&gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
gru_1/strided_slice_2Т
 gru_1/gru_cell_1/ones_like/ShapeShapegru_1/strided_slice_2:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/ones_like/ShapeЙ
 gru_1/gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2"
 gru_1/gru_cell_1/ones_like/Const»
gru_1/gru_cell_1/ones_likeFill)gru_1/gru_cell_1/ones_like/Shape:output:0)gru_1/gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/ones_likeМ
"gru_1/gru_cell_1/ones_like_1/ShapeShapegru_1/zeros:output:0*
T0*
_output_shapes
:2$
"gru_1/gru_cell_1/ones_like_1/ShapeН
"gru_1/gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_1/gru_cell_1/ones_like_1/Const–
gru_1/gru_cell_1/ones_like_1Fill+gru_1/gru_cell_1/ones_like_1/Shape:output:0+gru_1/gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/ones_like_1™
gru_1/gru_cell_1/mulMulgru_1/strided_slice_2:output:0#gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mulЃ
gru_1/gru_cell_1/mul_1Mulgru_1/strided_slice_2:output:0#gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mul_1Ѓ
gru_1/gru_cell_1/mul_2Mulgru_1/strided_slice_2:output:0#gru_1/gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mul_2Ђ
gru_1/gru_cell_1/ReadVariableOpReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02!
gru_1/gru_cell_1/ReadVariableOpЭ
$gru_1/gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2&
$gru_1/gru_cell_1/strided_slice/stack°
&gru_1/gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice/stack_1°
&gru_1/gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2(
&gru_1/gru_cell_1/strided_slice/stack_2в
gru_1/gru_cell_1/strided_sliceStridedSlice'gru_1/gru_cell_1/ReadVariableOp:value:0-gru_1/gru_cell_1/strided_slice/stack:output:0/gru_1/gru_cell_1/strided_slice/stack_1:output:0/gru_1/gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2 
gru_1/gru_cell_1/strided_slice±
gru_1/gru_cell_1/MatMulMatMulgru_1/gru_cell_1/mul:z:0'gru_1/gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMulѓ
!gru_1/gru_cell_1/ReadVariableOp_1ReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_1°
&gru_1/gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_1/stack•
(gru_1/gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2*
(gru_1/gru_cell_1/strided_slice_1/stack_1•
(gru_1/gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_1/stack_2о
 gru_1/gru_cell_1/strided_slice_1StridedSlice)gru_1/gru_cell_1/ReadVariableOp_1:value:0/gru_1/gru_cell_1/strided_slice_1/stack:output:01gru_1/gru_cell_1/strided_slice_1/stack_1:output:01gru_1/gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_1є
gru_1/gru_cell_1/MatMul_1MatMulgru_1/gru_cell_1/mul_1:z:0)gru_1/gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_1ѓ
!gru_1/gru_cell_1/ReadVariableOp_2ReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_2°
&gru_1/gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2(
&gru_1/gru_cell_1/strided_slice_2/stack•
(gru_1/gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_2/stack_1•
(gru_1/gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_2/stack_2о
 gru_1/gru_cell_1/strided_slice_2StridedSlice)gru_1/gru_cell_1/ReadVariableOp_2:value:0/gru_1/gru_cell_1/strided_slice_2/stack:output:01gru_1/gru_cell_1/strided_slice_2/stack_1:output:01gru_1/gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_2є
gru_1/gru_cell_1/MatMul_2MatMulgru_1/gru_cell_1/mul_2:z:0)gru_1/gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_2≠
!gru_1/gru_cell_1/ReadVariableOp_3ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_3Ъ
&gru_1/gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&gru_1/gru_cell_1/strided_slice_3/stackЮ
(gru_1/gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2*
(gru_1/gru_cell_1/strided_slice_3/stack_1Ю
(gru_1/gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_3/stack_2Џ
 gru_1/gru_cell_1/strided_slice_3StridedSlice)gru_1/gru_cell_1/ReadVariableOp_3:value:0/gru_1/gru_cell_1/strided_slice_3/stack:output:01gru_1/gru_cell_1/strided_slice_3/stack_1:output:01gru_1/gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2"
 gru_1/gru_cell_1/strided_slice_3њ
gru_1/gru_cell_1/BiasAddBiasAdd!gru_1/gru_cell_1/MatMul:product:0)gru_1/gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd≠
!gru_1/gru_cell_1/ReadVariableOp_4ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_4Ъ
&gru_1/gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&gru_1/gru_cell_1/strided_slice_4/stackЮ
(gru_1/gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2*
(gru_1/gru_cell_1/strided_slice_4/stack_1Ю
(gru_1/gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_4/stack_2»
 gru_1/gru_cell_1/strided_slice_4StridedSlice)gru_1/gru_cell_1/ReadVariableOp_4:value:0/gru_1/gru_cell_1/strided_slice_4/stack:output:01gru_1/gru_cell_1/strided_slice_4/stack_1:output:01gru_1/gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2"
 gru_1/gru_cell_1/strided_slice_4≈
gru_1/gru_cell_1/BiasAdd_1BiasAdd#gru_1/gru_cell_1/MatMul_1:product:0)gru_1/gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd_1≠
!gru_1/gru_cell_1/ReadVariableOp_5ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_5Ъ
&gru_1/gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2(
&gru_1/gru_cell_1/strided_slice_5/stackЮ
(gru_1/gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2*
(gru_1/gru_cell_1/strided_slice_5/stack_1Ю
(gru_1/gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_5/stack_2Ў
 gru_1/gru_cell_1/strided_slice_5StridedSlice)gru_1/gru_cell_1/ReadVariableOp_5:value:0/gru_1/gru_cell_1/strided_slice_5/stack:output:01gru_1/gru_cell_1/strided_slice_5/stack_1:output:01gru_1/gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2"
 gru_1/gru_cell_1/strided_slice_5≈
gru_1/gru_cell_1/BiasAdd_2BiasAdd#gru_1/gru_cell_1/MatMul_2:product:0)gru_1/gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd_2¶
gru_1/gru_cell_1/mul_3Mulgru_1/zeros:output:0%gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_3¶
gru_1/gru_cell_1/mul_4Mulgru_1/zeros:output:0%gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_4¶
gru_1/gru_cell_1/mul_5Mulgru_1/zeros:output:0%gru_1/gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_5±
!gru_1/gru_cell_1/ReadVariableOp_6ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_6°
&gru_1/gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_6/stack•
(gru_1/gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_6/stack_1•
(gru_1/gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_6/stack_2о
 gru_1/gru_cell_1/strided_slice_6StridedSlice)gru_1/gru_cell_1/ReadVariableOp_6:value:0/gru_1/gru_cell_1/strided_slice_6/stack:output:01gru_1/gru_cell_1/strided_slice_6/stack_1:output:01gru_1/gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_6є
gru_1/gru_cell_1/MatMul_3MatMulgru_1/gru_cell_1/mul_3:z:0)gru_1/gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_3±
!gru_1/gru_cell_1/ReadVariableOp_7ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_7°
&gru_1/gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_7/stack•
(gru_1/gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2*
(gru_1/gru_cell_1/strided_slice_7/stack_1•
(gru_1/gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_7/stack_2о
 gru_1/gru_cell_1/strided_slice_7StridedSlice)gru_1/gru_cell_1/ReadVariableOp_7:value:0/gru_1/gru_cell_1/strided_slice_7/stack:output:01gru_1/gru_cell_1/strided_slice_7/stack_1:output:01gru_1/gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_7є
gru_1/gru_cell_1/MatMul_4MatMulgru_1/gru_cell_1/mul_4:z:0)gru_1/gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_4ѓ
gru_1/gru_cell_1/addAddV2!gru_1/gru_cell_1/BiasAdd:output:0#gru_1/gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/addu
gru_1/gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_1/gru_cell_1/Consty
gru_1/gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_1/gru_cell_1/Const_1§
gru_1/gru_cell_1/Mul_6Mulgru_1/gru_cell_1/add:z:0gru_1/gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Mul_6®
gru_1/gru_cell_1/Add_1Addgru_1/gru_cell_1/Mul_6:z:0!gru_1/gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Add_1Щ
(gru_1/gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2*
(gru_1/gru_cell_1/clip_by_value/Minimum/y№
&gru_1/gru_cell_1/clip_by_value/MinimumMinimumgru_1/gru_cell_1/Add_1:z:01gru_1/gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&gru_1/gru_cell_1/clip_by_value/MinimumЙ
 gru_1/gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2"
 gru_1/gru_cell_1/clip_by_value/y‘
gru_1/gru_cell_1/clip_by_valueMaximum*gru_1/gru_cell_1/clip_by_value/Minimum:z:0)gru_1/gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/gru_cell_1/clip_by_valueµ
gru_1/gru_cell_1/add_2AddV2#gru_1/gru_cell_1/BiasAdd_1:output:0#gru_1/gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_2y
gru_1/gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_1/gru_cell_1/Const_2y
gru_1/gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_1/gru_cell_1/Const_3®
gru_1/gru_cell_1/Mul_7Mulgru_1/gru_cell_1/add_2:z:0!gru_1/gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Mul_7®
gru_1/gru_cell_1/Add_3Addgru_1/gru_cell_1/Mul_7:z:0!gru_1/gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Add_3Э
*gru_1/gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2,
*gru_1/gru_cell_1/clip_by_value_1/Minimum/yв
(gru_1/gru_cell_1/clip_by_value_1/MinimumMinimumgru_1/gru_cell_1/Add_3:z:03gru_1/gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2*
(gru_1/gru_cell_1/clip_by_value_1/MinimumН
"gru_1/gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"gru_1/gru_cell_1/clip_by_value_1/y№
 gru_1/gru_cell_1/clip_by_value_1Maximum,gru_1/gru_cell_1/clip_by_value_1/Minimum:z:0+gru_1/gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/gru_cell_1/clip_by_value_1Ђ
gru_1/gru_cell_1/mul_8Mul$gru_1/gru_cell_1/clip_by_value_1:z:0gru_1/gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_8±
!gru_1/gru_cell_1/ReadVariableOp_8ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_8°
&gru_1/gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2(
&gru_1/gru_cell_1/strided_slice_8/stack•
(gru_1/gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_8/stack_1•
(gru_1/gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_8/stack_2о
 gru_1/gru_cell_1/strided_slice_8StridedSlice)gru_1/gru_cell_1/ReadVariableOp_8:value:0/gru_1/gru_cell_1/strided_slice_8/stack:output:01gru_1/gru_cell_1/strided_slice_8/stack_1:output:01gru_1/gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_8є
gru_1/gru_cell_1/MatMul_5MatMulgru_1/gru_cell_1/mul_8:z:0)gru_1/gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_5µ
gru_1/gru_cell_1/add_4AddV2#gru_1/gru_cell_1/BiasAdd_2:output:0#gru_1/gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_4Д
gru_1/gru_cell_1/TanhTanhgru_1/gru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Tanh£
gru_1/gru_cell_1/mul_9Mul"gru_1/gru_cell_1/clip_by_value:z:0gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_9u
gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/gru_cell_1/sub/x™
gru_1/gru_cell_1/subSubgru_1/gru_cell_1/sub/x:output:0"gru_1/gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/sub†
gru_1/gru_cell_1/mul_10Mulgru_1/gru_cell_1/sub:z:0gru_1/gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_10§
gru_1/gru_cell_1/add_5AddV2gru_1/gru_cell_1/mul_9:z:0gru_1/gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_5Ы
#gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2%
#gru_1/TensorArrayV2_1/element_shape–
gru_1/TensorArrayV2_1TensorListReserve,gru_1/TensorArrayV2_1/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2_1Z

gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru_1/timeЛ
gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru_1/while/maximum_iterationsv
gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/while/loop_counterн
gru_1/whileWhile!gru_1/while/loop_counter:output:0'gru_1/while/maximum_iterations:output:0gru_1/time:output:0gru_1/TensorArrayV2_1:handle:0gru_1/zeros:output:0gru_1/strided_slice_1:output:0=gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:0(gru_1_gru_cell_1_readvariableop_resource*gru_1_gru_cell_1_readvariableop_3_resource*gru_1_gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*"
bodyR
gru_1_while_body_39079*"
condR
gru_1_while_cond_39078*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
gru_1/whileЅ
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    28
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeЙ
(gru_1/TensorArrayV2Stack/TensorListStackTensorListStackgru_1/while:output:3?gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02*
(gru_1/TensorArrayV2Stack/TensorListStackН
gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru_1/strided_slice_3/stackИ
gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_3/stack_1И
gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_3/stack_2Њ
gru_1/strided_slice_3StridedSlice1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0$gru_1/strided_slice_3/stack:output:0&gru_1/strided_slice_3/stack_1:output:0&gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
gru_1/strided_slice_3Е
gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose_1/perm∆
gru_1/transpose_1	Transpose1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0gru_1/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
gru_1/transpose_1r
gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/runtime•
dense_1/MatMul/ReadVariableOpReadVariableOp&dense_1_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense_1/MatMul/ReadVariableOp£
dense_1/MatMulMatMulgru_1/strided_slice_3:output:0%dense_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense_1/MatMul§
dense_1/BiasAdd/ReadVariableOpReadVariableOp'dense_1_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_1/BiasAdd/ReadVariableOp°
dense_1/BiasAddBiasAdddense_1/MatMul:product:0&dense_1/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense_1/BiasAddz
IdentityIdentitydense_1/BiasAdd:output:0^gru_1/while*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2
gru_1/whilegru_1/while:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ќ
Л
%__inference_gru_1_layer_call_fn_39935
inputs_0
unknown
	unknown_0
	unknown_1
identityИҐStatefulPartitionedCallЁ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_376172
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
£≤
г
@__inference_gru_1_layer_call_and_return_conditional_losses_38405

inputs&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likez
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1Т
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЦ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ц
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2О
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3О
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4О
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_38254*
condR
while_cond_38253*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
—
Ѓ
,__inference_sequential_1_layer_call_fn_39251

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identityИҐStatefulPartitionedCallь
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*P
fKRI
G__inference_sequential_1_layer_call_and_return_conditional_losses_384972
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ЪЫ
Ё
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40794

inputs
states_0
readvariableop_resource
readvariableop_3_resource
readvariableop_6_resource
identity

identity_1ИX
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2
ones_like/Shapeg
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like/ConstД
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
	ones_likec
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout/Const
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/Mul`
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout/Shape“
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ВзW2&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout/GreaterEqual/yЊ
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/GreaterEqual
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout/Castz
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/Mul_1g
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout_1/ConstЕ
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/Muld
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout_1/ShapeЎ
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ЭЕ{2(
&dropout_1/random_uniform/RandomUniformy
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout_1/GreaterEqual/y∆
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/GreaterEqualЕ
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout_1/CastВ
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/Mul_1g
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout_2/ConstЕ
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/Muld
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout_2/Shapeў
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2Йґ≥2(
&dropout_2/random_uniform/RandomUniformy
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout_2/GreaterEqual/y∆
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/GreaterEqualЕ
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout_2/CastВ
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/Mul_1^
ones_like_1/ShapeShapestates_0*
T0*
_output_shapes
:2
ones_like_1/Shapek
ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like_1/ConstМ
ones_like_1Fillones_like_1/Shape:output:0ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
ones_like_1g
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_3/ConstЗ
dropout_3/MulMulones_like_1:output:0dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/Mulf
dropout_3/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_3/Shapeў
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ЌЬќ2(
&dropout_3/random_uniform/RandomUniformy
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_3/GreaterEqual/y∆
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/GreaterEqualЕ
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/CastВ
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/Mul_1g
dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_4/ConstЗ
dropout_4/MulMulones_like_1:output:0dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/Mulf
dropout_4/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_4/Shapeў
&dropout_4/random_uniform/RandomUniformRandomUniformdropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2НџГ2(
&dropout_4/random_uniform/RandomUniformy
dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_4/GreaterEqual/y∆
dropout_4/GreaterEqualGreaterEqual/dropout_4/random_uniform/RandomUniform:output:0!dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/GreaterEqualЕ
dropout_4/CastCastdropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/CastВ
dropout_4/Mul_1Muldropout_4/Mul:z:0dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/Mul_1g
dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_5/ConstЗ
dropout_5/MulMulones_like_1:output:0dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/Mulf
dropout_5/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_5/Shapeў
&dropout_5/random_uniform/RandomUniformRandomUniformdropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2–¬я2(
&dropout_5/random_uniform/RandomUniformy
dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_5/GreaterEqual/y∆
dropout_5/GreaterEqualGreaterEqual/dropout_5/random_uniform/RandomUniform:output:0!dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/GreaterEqualЕ
dropout_5/CastCastdropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/CastВ
dropout_5/Mul_1Muldropout_5/Mul:z:0dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/Mul_1^
mulMulinputsdropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
muld
mul_1Mulinputsdropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_1d
mul_2Mulinputsdropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_2x
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ь
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slicem
MatMulMatMulmul:z:0strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
MatMul|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stackГ
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack_1Г
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2И
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_1u
MatMul_1MatMul	mul_1:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_1|
ReadVariableOp_2ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_2
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_2/stackГ
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_2/stack_1Г
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_2/stack_2И
strided_slice_2StridedSliceReadVariableOp_2:value:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_2u
MatMul_2MatMul	mul_2:z:0strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_2z
ReadVariableOp_3ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_3x
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2ф
strided_slice_3StridedSliceReadVariableOp_3:value:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
strided_slice_3{
BiasAddBiasAddMatMul:product:0strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
BiasAddz
ReadVariableOp_4ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_4x
strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_4/stack|
strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_4/stack_1|
strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_4/stack_2в
strided_slice_4StridedSliceReadVariableOp_4:value:0strided_slice_4/stack:output:0 strided_slice_4/stack_1:output:0 strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
strided_slice_4Б
	BiasAdd_1BiasAddMatMul_1:product:0strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_1z
ReadVariableOp_5ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_5x
strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_5/stack|
strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_5/stack_1|
strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_5/stack_2т
strided_slice_5StridedSliceReadVariableOp_5:value:0strided_slice_5/stack:output:0 strided_slice_5/stack_1:output:0 strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
strided_slice_5Б
	BiasAdd_2BiasAddMatMul_2:product:0strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_2f
mul_3Mulstates_0dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_3f
mul_4Mulstates_0dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_4f
mul_5Mulstates_0dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_5~
ReadVariableOp_6ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_6
strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stackГ
strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stack_1Г
strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_6/stack_2И
strided_slice_6StridedSliceReadVariableOp_6:value:0strided_slice_6/stack:output:0 strided_slice_6/stack_1:output:0 strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_6u
MatMul_3MatMul	mul_3:z:0strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_3~
ReadVariableOp_7ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_7
strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_7/stackГ
strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_7/stack_1Г
strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_7/stack_2И
strided_slice_7StridedSliceReadVariableOp_7:value:0strided_slice_7/stack:output:0 strided_slice_7/stack_1:output:0 strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_7u
MatMul_4MatMul	mul_4:z:0strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_4k
addAddV2BiasAdd:output:0MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1`
Mul_6Muladd:z:0Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_6d
Add_1Add	Mul_6:z:0Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value/Minimum/yШ
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/yР
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_valueq
add_2AddV2BiasAdd_1:output:0MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2W
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3d
Mul_7Mul	add_2:z:0Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_7d
Add_3Add	Mul_7:z:0Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value_1/Minimum/yЮ
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/yШ
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1g
mul_8Mulclip_by_value_1:z:0	mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_8~
ReadVariableOp_8ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_8
strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_8/stackГ
strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_8/stack_1Г
strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_8/stack_2И
strided_slice_8StridedSliceReadVariableOp_8:value:0strided_slice_8/stack:output:0 strided_slice_8/stack_1:output:0 strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_8u
MatMul_5MatMul	mul_8:z:0strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_5q
add_4AddV2BiasAdd_2:output:0MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Tanhd
mul_9Mulclip_by_value:z:0states_0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_9S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sub\
mul_10Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_10`
add_5AddV2	mul_9:z:0
mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ ::::O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Ќ
Л
%__inference_gru_1_layer_call_fn_39946
inputs_0
unknown
	unknown_0
	unknown_1
identityИҐStatefulPartitionedCallЁ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_377352
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
Ж
G__inference_sequential_1_layer_call_and_return_conditional_losses_38478
gru_1_input
gru_1_38465
gru_1_38467
gru_1_38469
dense_1_38472
dense_1_38474
identityИҐdense_1/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallф
gru_1/StatefulPartitionedCallStatefulPartitionedCallgru_1_inputgru_1_38465gru_1_38467gru_1_38469*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_384052
gru_1/StatefulPartitionedCallК
dense_1/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_1_38472dense_1_38474*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*K
fFRD
B__inference_dense_1_layer_call_and_return_conditional_losses_384452!
dense_1/StatefulPartitionedCallЊ
IdentityIdentity(dense_1/StatefulPartitionedCall:output:0 ^dense_1/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
∞
™
#__inference_signature_wrapper_38566
gru_1_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identityИҐStatefulPartitionedCallЏ
StatefulPartitionedCallStatefulPartitionedCallgru_1_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*)
f$R"
 __inference__wrapped_model_370122
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::22
StatefulPartitionedCallStatefulPartitionedCall:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
‘Т
Ю
while_body_40453
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1§
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul®
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1®
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
Ђ≤
е
@__inference_gru_1_layer_call_and_return_conditional_losses_39924
inputs_0&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЕ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likez
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1Т
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЦ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ц
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2О
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3О
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4О
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_39773*
condR
while_cond_39772*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Эe
џ
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_37294

inputs

states
readvariableop_resource
readvariableop_3_resource
readvariableop_6_resource
identity

identity_1ИX
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2
ones_like/Shapeg
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like/ConstД
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
	ones_like\
ones_like_1/ShapeShapestates*
T0*
_output_shapes
:2
ones_like_1/Shapek
ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like_1/ConstМ
ones_like_1Fillones_like_1/Shape:output:0ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
ones_like_1_
mulMulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mulc
mul_1Mulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_1c
mul_2Mulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_2x
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ь
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slicem
MatMulMatMulmul:z:0strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
MatMul|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stackГ
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack_1Г
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2И
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_1u
MatMul_1MatMul	mul_1:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_1|
ReadVariableOp_2ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_2
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_2/stackГ
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_2/stack_1Г
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_2/stack_2И
strided_slice_2StridedSliceReadVariableOp_2:value:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_2u
MatMul_2MatMul	mul_2:z:0strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_2z
ReadVariableOp_3ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_3x
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2ф
strided_slice_3StridedSliceReadVariableOp_3:value:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
strided_slice_3{
BiasAddBiasAddMatMul:product:0strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
BiasAddz
ReadVariableOp_4ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_4x
strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_4/stack|
strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_4/stack_1|
strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_4/stack_2в
strided_slice_4StridedSliceReadVariableOp_4:value:0strided_slice_4/stack:output:0 strided_slice_4/stack_1:output:0 strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
strided_slice_4Б
	BiasAdd_1BiasAddMatMul_1:product:0strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_1z
ReadVariableOp_5ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_5x
strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_5/stack|
strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_5/stack_1|
strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_5/stack_2т
strided_slice_5StridedSliceReadVariableOp_5:value:0strided_slice_5/stack:output:0 strided_slice_5/stack_1:output:0 strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
strided_slice_5Б
	BiasAdd_2BiasAddMatMul_2:product:0strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_2e
mul_3Mulstatesones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_3e
mul_4Mulstatesones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_4e
mul_5Mulstatesones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_5~
ReadVariableOp_6ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_6
strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stackГ
strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stack_1Г
strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_6/stack_2И
strided_slice_6StridedSliceReadVariableOp_6:value:0strided_slice_6/stack:output:0 strided_slice_6/stack_1:output:0 strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_6u
MatMul_3MatMul	mul_3:z:0strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_3~
ReadVariableOp_7ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_7
strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_7/stackГ
strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_7/stack_1Г
strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_7/stack_2И
strided_slice_7StridedSliceReadVariableOp_7:value:0strided_slice_7/stack:output:0 strided_slice_7/stack_1:output:0 strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_7u
MatMul_4MatMul	mul_4:z:0strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_4k
addAddV2BiasAdd:output:0MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1`
Mul_6Muladd:z:0Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_6d
Add_1Add	Mul_6:z:0Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value/Minimum/yШ
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/yР
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_valueq
add_2AddV2BiasAdd_1:output:0MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2W
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3d
Mul_7Mul	add_2:z:0Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_7d
Add_3Add	Mul_7:z:0Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value_1/Minimum/yЮ
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/yШ
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1g
mul_8Mulclip_by_value_1:z:0	mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_8~
ReadVariableOp_8ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_8
strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_8/stackГ
strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_8/stack_1Г
strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_8/stack_2И
strided_slice_8StridedSliceReadVariableOp_8:value:0strided_slice_8/stack:output:0 strided_slice_8/stack_1:output:0 strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_8u
MatMul_5MatMul	mul_8:z:0strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_5q
add_4AddV2BiasAdd_2:output:0MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Tanhb
mul_9Mulclip_by_value:z:0states*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_9S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sub\
mul_10Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_10`
add_5AddV2	mul_9:z:0
mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ ::::O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:OK
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_namestates:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ё
ы
#sequential_1_gru_1_while_cond_36854)
%sequential_1_gru_1_while_loop_counter/
+sequential_1_gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2+
'less_sequential_1_gru_1_strided_slice_1@
<sequential_1_gru_1_while_cond_36854___redundant_placeholder0@
<sequential_1_gru_1_while_cond_36854___redundant_placeholder1@
<sequential_1_gru_1_while_cond_36854___redundant_placeholder2@
<sequential_1_gru_1_while_cond_36854___redundant_placeholder3
identity
k
LessLessplaceholder'less_sequential_1_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
т
|
'__inference_dense_1_layer_call_fn_40645

inputs
unknown
	unknown_0
identityИҐStatefulPartitionedCall–
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*K
fFRD
B__inference_dense_1_layer_call_and_return_conditional_losses_384452
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*.
_input_shapes
:€€€€€€€€€ ::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: 
Ђe
Ё
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40895

inputs
states_0
readvariableop_resource
readvariableop_3_resource
readvariableop_6_resource
identity

identity_1ИX
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2
ones_like/Shapeg
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like/ConstД
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
	ones_like^
ones_like_1/ShapeShapestates_0*
T0*
_output_shapes
:2
ones_like_1/Shapek
ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like_1/ConstМ
ones_like_1Fillones_like_1/Shape:output:0ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
ones_like_1_
mulMulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mulc
mul_1Mulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_1c
mul_2Mulinputsones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_2x
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ь
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slicem
MatMulMatMulmul:z:0strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
MatMul|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stackГ
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack_1Г
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2И
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_1u
MatMul_1MatMul	mul_1:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_1|
ReadVariableOp_2ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_2
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_2/stackГ
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_2/stack_1Г
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_2/stack_2И
strided_slice_2StridedSliceReadVariableOp_2:value:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_2u
MatMul_2MatMul	mul_2:z:0strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_2z
ReadVariableOp_3ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_3x
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2ф
strided_slice_3StridedSliceReadVariableOp_3:value:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
strided_slice_3{
BiasAddBiasAddMatMul:product:0strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
BiasAddz
ReadVariableOp_4ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_4x
strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_4/stack|
strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_4/stack_1|
strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_4/stack_2в
strided_slice_4StridedSliceReadVariableOp_4:value:0strided_slice_4/stack:output:0 strided_slice_4/stack_1:output:0 strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
strided_slice_4Б
	BiasAdd_1BiasAddMatMul_1:product:0strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_1z
ReadVariableOp_5ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_5x
strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_5/stack|
strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_5/stack_1|
strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_5/stack_2т
strided_slice_5StridedSliceReadVariableOp_5:value:0strided_slice_5/stack:output:0 strided_slice_5/stack_1:output:0 strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
strided_slice_5Б
	BiasAdd_2BiasAddMatMul_2:product:0strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_2g
mul_3Mulstates_0ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_3g
mul_4Mulstates_0ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_4g
mul_5Mulstates_0ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_5~
ReadVariableOp_6ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_6
strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stackГ
strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stack_1Г
strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_6/stack_2И
strided_slice_6StridedSliceReadVariableOp_6:value:0strided_slice_6/stack:output:0 strided_slice_6/stack_1:output:0 strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_6u
MatMul_3MatMul	mul_3:z:0strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_3~
ReadVariableOp_7ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_7
strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_7/stackГ
strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_7/stack_1Г
strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_7/stack_2И
strided_slice_7StridedSliceReadVariableOp_7:value:0strided_slice_7/stack:output:0 strided_slice_7/stack_1:output:0 strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_7u
MatMul_4MatMul	mul_4:z:0strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_4k
addAddV2BiasAdd:output:0MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1`
Mul_6Muladd:z:0Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_6d
Add_1Add	Mul_6:z:0Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value/Minimum/yШ
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/yР
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_valueq
add_2AddV2BiasAdd_1:output:0MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2W
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3d
Mul_7Mul	add_2:z:0Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_7d
Add_3Add	Mul_7:z:0Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value_1/Minimum/yЮ
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/yШ
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1g
mul_8Mulclip_by_value_1:z:0	mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_8~
ReadVariableOp_8ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_8
strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_8/stackГ
strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_8/stack_1Г
strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_8/stack_2И
strided_slice_8StridedSliceReadVariableOp_8:value:0strided_slice_8/stack:output:0 strided_slice_8/stack_1:output:0 strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_8u
MatMul_5MatMul	mul_8:z:0strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_5q
add_4AddV2BiasAdd_2:output:0MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Tanhd
mul_9Mulclip_by_value:z:0states_0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_9S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sub\
mul_10Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_10`
add_5AddV2	mul_9:z:0
mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ ::::O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
§
Б
G__inference_sequential_1_layer_call_and_return_conditional_losses_38497

inputs
gru_1_38484
gru_1_38486
gru_1_38488
dense_1_38491
dense_1_38493
identityИҐdense_1/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallп
gru_1/StatefulPartitionedCallStatefulPartitionedCallinputsgru_1_38484gru_1_38486gru_1_38488*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_381242
gru_1/StatefulPartitionedCallК
dense_1/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_1_38491dense_1_38493*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*K
fFRD
B__inference_dense_1_layer_call_and_return_conditional_losses_384452!
dense_1/StatefulPartitionedCallЊ
IdentityIdentity(dense_1/StatefulPartitionedCall:output:0 ^dense_1/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
‘Т
Ю
while_body_38254
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1§
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul®
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1®
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
ёФ
£
#sequential_1_gru_1_while_body_36855)
%sequential_1_gru_1_while_loop_counter/
+sequential_1_gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2(
$sequential_1_gru_1_strided_slice_1_0d
`tensorarrayv2read_tensorlistgetitem_sequential_1_gru_1_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4&
"sequential_1_gru_1_strided_slice_1b
^tensorarrayv2read_tensorlistgetitem_sequential_1_gru_1_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shape»
#TensorArrayV2Read/TensorListGetItemTensorListGetItem`tensorarrayv2read_tensorlistgetitem_sequential_1_gru_1_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1§
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul®
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1®
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/yq
add_1AddV2%sequential_1_gru_1_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identityr

Identity_1Identity+sequential_1_gru_1_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"J
"sequential_1_gru_1_strided_slice_1$sequential_1_gru_1_strided_slice_1_0"¬
^tensorarrayv2read_tensorlistgetitem_sequential_1_gru_1_tensorarrayunstack_tensorlistfromtensor`tensorarrayv2read_tensorlistgetitem_sequential_1_gru_1_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
а
≥
,__inference_sequential_1_layer_call_fn_38510
gru_1_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identityИҐStatefulPartitionedCallБ
StatefulPartitionedCallStatefulPartitionedCallgru_1_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*P
fKRI
G__inference_sequential_1_layer_call_and_return_conditional_losses_384972
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::22
StatefulPartitionedCallStatefulPartitionedCall:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
г
while_cond_39443
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_39443___redundant_placeholder0-
)while_cond_39443___redundant_placeholder1-
)while_cond_39443___redundant_placeholder2-
)while_cond_39443___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
§
Б
G__inference_sequential_1_layer_call_and_return_conditional_losses_38528

inputs
gru_1_38515
gru_1_38517
gru_1_38519
dense_1_38522
dense_1_38524
identityИҐdense_1/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallп
gru_1/StatefulPartitionedCallStatefulPartitionedCallinputsgru_1_38515gru_1_38517gru_1_38519*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_384052
gru_1/StatefulPartitionedCallК
dense_1/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_1_38522dense_1_38524*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*K
fFRD
B__inference_dense_1_layer_call_and_return_conditional_losses_384452!
dense_1/StatefulPartitionedCallЊ
IdentityIdentity(dense_1/StatefulPartitionedCall:output:0 ^dense_1/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
г
while_cond_37924
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_37924___redundant_placeholder0-
)while_cond_37924___redundant_placeholder1-
)while_cond_37924___redundant_placeholder2-
)while_cond_37924___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Ух
г
@__inference_gru_1_layer_call_and_return_conditional_losses_40323

inputs&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2шзљ21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeщ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2йфm23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2р¬е23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1z
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2тЪ¬23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeщ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2Рљ	23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2√тЭ23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1С
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЧ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ч
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_40124*
condR
while_cond_40123*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ЛЫ
џ
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_37193

inputs

states
readvariableop_resource
readvariableop_3_resource
readvariableop_6_resource
identity

identity_1ИX
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2
ones_like/Shapeg
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like/ConstД
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
	ones_likec
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout/Const
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/Mul`
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout/Shape“
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2фЯ2&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout/GreaterEqual/yЊ
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/GreaterEqual
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout/Castz
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout/Mul_1g
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout_1/ConstЕ
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/Muld
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout_1/Shapeў
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2§µЗ2(
&dropout_1/random_uniform/RandomUniformy
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout_1/GreaterEqual/y∆
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/GreaterEqualЕ
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout_1/CastВ
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_1/Mul_1g
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
dropout_2/ConstЕ
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/Muld
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:2
dropout_2/Shapeў
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2§АЋ2(
&dropout_2/random_uniform/RandomUniformy
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
dropout_2/GreaterEqual/y∆
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/GreaterEqualЕ
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
dropout_2/CastВ
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
dropout_2/Mul_1\
ones_like_1/ShapeShapestates*
T0*
_output_shapes
:2
ones_like_1/Shapek
ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
ones_like_1/ConstМ
ones_like_1Fillones_like_1/Shape:output:0ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
ones_like_1g
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_3/ConstЗ
dropout_3/MulMulones_like_1:output:0dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/Mulf
dropout_3/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_3/Shapeў
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2НЅп2(
&dropout_3/random_uniform/RandomUniformy
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_3/GreaterEqual/y∆
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/GreaterEqualЕ
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/CastВ
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_3/Mul_1g
dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_4/ConstЗ
dropout_4/MulMulones_like_1:output:0dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/Mulf
dropout_4/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_4/ShapeЎ
&dropout_4/random_uniform/RandomUniformRandomUniformdropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2њ«2(
&dropout_4/random_uniform/RandomUniformy
dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_4/GreaterEqual/y∆
dropout_4/GreaterEqualGreaterEqual/dropout_4/random_uniform/RandomUniform:output:0!dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/GreaterEqualЕ
dropout_4/CastCastdropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/CastВ
dropout_4/Mul_1Muldropout_4/Mul:z:0dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_4/Mul_1g
dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
dropout_5/ConstЗ
dropout_5/MulMulones_like_1:output:0dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/Mulf
dropout_5/ShapeShapeones_like_1:output:0*
T0*
_output_shapes
:2
dropout_5/ShapeЎ
&dropout_5/random_uniform/RandomUniformRandomUniformdropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2еКc2(
&dropout_5/random_uniform/RandomUniformy
dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2
dropout_5/GreaterEqual/y∆
dropout_5/GreaterEqualGreaterEqual/dropout_5/random_uniform/RandomUniform:output:0!dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/GreaterEqualЕ
dropout_5/CastCastdropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/CastВ
dropout_5/Mul_1Muldropout_5/Mul:z:0dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
dropout_5/Mul_1^
mulMulinputsdropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
muld
mul_1Mulinputsdropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_1d
mul_2Mulinputsdropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
mul_2x
ReadVariableOpReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp{
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice/stack_1
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice/stack_2ь
strided_sliceStridedSliceReadVariableOp:value:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slicem
MatMulMatMulmul:z:0strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
MatMul|
ReadVariableOp_1ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_1
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_1/stackГ
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_1/stack_1Г
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_1/stack_2И
strided_slice_1StridedSliceReadVariableOp_1:value:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_1u
MatMul_1MatMul	mul_1:z:0strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_1|
ReadVariableOp_2ReadVariableOpreadvariableop_resource*
_output_shapes

:`*
dtype02
ReadVariableOp_2
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_2/stackГ
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_2/stack_1Г
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_2/stack_2И
strided_slice_2StridedSliceReadVariableOp_2:value:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
strided_slice_2u
MatMul_2MatMul	mul_2:z:0strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_2z
ReadVariableOp_3ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_3x
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2ф
strided_slice_3StridedSliceReadVariableOp_3:value:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
strided_slice_3{
BiasAddBiasAddMatMul:product:0strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2	
BiasAddz
ReadVariableOp_4ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_4x
strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_4/stack|
strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_4/stack_1|
strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_4/stack_2в
strided_slice_4StridedSliceReadVariableOp_4:value:0strided_slice_4/stack:output:0 strided_slice_4/stack_1:output:0 strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
strided_slice_4Б
	BiasAdd_1BiasAddMatMul_1:product:0strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_1z
ReadVariableOp_5ReadVariableOpreadvariableop_3_resource*
_output_shapes
:`*
dtype02
ReadVariableOp_5x
strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2
strided_slice_5/stack|
strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_5/stack_1|
strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_5/stack_2т
strided_slice_5StridedSliceReadVariableOp_5:value:0strided_slice_5/stack:output:0 strided_slice_5/stack_1:output:0 strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
strided_slice_5Б
	BiasAdd_2BiasAddMatMul_2:product:0strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
	BiasAdd_2d
mul_3Mulstatesdropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_3d
mul_4Mulstatesdropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_4d
mul_5Mulstatesdropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_5~
ReadVariableOp_6ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_6
strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stackГ
strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_6/stack_1Г
strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_6/stack_2И
strided_slice_6StridedSliceReadVariableOp_6:value:0strided_slice_6/stack:output:0 strided_slice_6/stack_1:output:0 strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_6u
MatMul_3MatMul	mul_3:z:0strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_3~
ReadVariableOp_7ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_7
strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_7/stackГ
strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_7/stack_1Г
strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_7/stack_2И
strided_slice_7StridedSliceReadVariableOp_7:value:0strided_slice_7/stack:output:0 strided_slice_7/stack_1:output:0 strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_7u
MatMul_4MatMul	mul_4:z:0strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_4k
addAddV2BiasAdd:output:0MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
addS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1`
Mul_6Muladd:z:0Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_6d
Add_1Add	Mul_6:z:0Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value/Minimum/yШ
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/yР
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_valueq
add_2AddV2BiasAdd_1:output:0MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_2W
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3d
Mul_7Mul	add_2:z:0Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Mul_7d
Add_3Add	Mul_7:z:0Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Add_3{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
clip_by_value_1/Minimum/yЮ
clip_by_value_1/MinimumMinimum	Add_3:z:0"clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/yШ
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
clip_by_value_1g
mul_8Mulclip_by_value_1:z:0	mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_8~
ReadVariableOp_8ReadVariableOpreadvariableop_6_resource*
_output_shapes

: `*
dtype02
ReadVariableOp_8
strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2
strided_slice_8/stackГ
strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2
strided_slice_8/stack_1Г
strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2
strided_slice_8/stack_2И
strided_slice_8StridedSliceReadVariableOp_8:value:0strided_slice_8/stack:output:0 strided_slice_8/stack_1:output:0 strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
strided_slice_8u
MatMul_5MatMul	mul_8:z:0strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

MatMul_5q
add_4AddV2BiasAdd_2:output:0MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_4Q
TanhTanh	add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
Tanhb
mul_9Mulclip_by_value:z:0states*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_9S
sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
sub/xf
subSubsub/x:output:0clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
sub\
mul_10Mulsub:z:0Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
mul_10`
add_5AddV2	mul_9:z:0
mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
add_5]
IdentityIdentity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identitya

Identity_1Identity	add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ ::::O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:OK
'
_output_shapes
:€€€€€€€€€ 
 
_user_specified_namestates:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
й
У
gru_1_while_cond_38743
gru_1_while_loop_counter"
gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_gru_1_strided_slice_13
/gru_1_while_cond_38743___redundant_placeholder03
/gru_1_while_cond_38743___redundant_placeholder13
/gru_1_while_cond_38743___redundant_placeholder23
/gru_1_while_cond_38743___redundant_placeholder3
identity
^
LessLessplaceholderless_gru_1_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
√
ѕ
while_body_37553
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
gru_cell_1_37575_0
gru_cell_1_37577_0
gru_cell_1_37579_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
gru_cell_1_37575
gru_cell_1_37577
gru_cell_1_37579ИҐ"gru_cell_1/StatefulPartitionedCallЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemџ
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCall*TensorArrayV2Read/TensorListGetItem:item:0placeholder_2gru_cell_1_37575_0gru_cell_1_37577_0gru_cell_1_37579_0*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_371932$
"gru_cell_1/StatefulPartitionedCall„
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholder+gru_cell_1/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1q
IdentityIdentity	add_1:z:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

IdentityД

Identity_1Identitywhile_maximum_iterations#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_1s

Identity_2Identityadd:z:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_2†

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_3®

Identity_4Identity+gru_cell_1/StatefulPartitionedCall:output:1#^gru_cell_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"&
gru_cell_1_37575gru_cell_1_37575_0"&
gru_cell_1_37577gru_cell_1_37577_0"&
gru_cell_1_37579gru_cell_1_37579_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
£≤
г
@__inference_gru_1_layer_call_and_return_conditional_losses_40604

inputs&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likez
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1Т
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЦ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ц
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2О
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3О
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4О
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_40453*
condR
while_cond_40452*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≈’
Ю
while_body_40124
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2юјя21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeщ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2Д≈E23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2°—Ч23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1y
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2≥зЅ23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeъ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2µд≥23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ЗµС23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1£
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul©
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1©
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2М
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3М
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4М
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
—
Ѓ
,__inference_sequential_1_layer_call_fn_39266

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
identityИҐStatefulPartitionedCallь
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*'
_output_shapes
:€€€€€€€€€*'
_read_only_resource_inputs	
**
config_proto

CPU

GPU 2J 8*P
fKRI
G__inference_sequential_1_layer_call_and_return_conditional_losses_385282
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
√
ѕ
while_body_37671
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
gru_cell_1_37693_0
gru_cell_1_37695_0
gru_cell_1_37697_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
gru_cell_1_37693
gru_cell_1_37695
gru_cell_1_37697ИҐ"gru_cell_1/StatefulPartitionedCallЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemџ
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCall*TensorArrayV2Read/TensorListGetItem:item:0placeholder_2gru_cell_1_37693_0gru_cell_1_37695_0gru_cell_1_37697_0*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_372942$
"gru_cell_1/StatefulPartitionedCall„
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholder+gru_cell_1/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1q
IdentityIdentity	add_1:z:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

IdentityД

Identity_1Identitywhile_maximum_iterations#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_1s

Identity_2Identityadd:z:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_2†

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0#^gru_cell_1/StatefulPartitionedCall*
T0*
_output_shapes
: 2

Identity_3®

Identity_4Identity+gru_cell_1/StatefulPartitionedCall:output:1#^gru_cell_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"&
gru_cell_1_37693gru_cell_1_37693_0"&
gru_cell_1_37695gru_cell_1_37695_0"&
gru_cell_1_37697gru_cell_1_37697_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
≈’
Ю
while_body_37925
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeу
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2У‘P21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeъ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2Ъ®ї23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ћиј23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1y
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2«ѕј23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeъ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2нЗх23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2°ыэ23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1£
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul©
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1©
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2М
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3М
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4М
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
≥
г
while_cond_40452
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_40452___redundant_placeholder0-
)while_cond_40452___redundant_placeholder1-
)while_cond_40452___redundant_placeholder2-
)while_cond_40452___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
Т

ђ
*__inference_gru_cell_1_layer_call_fn_40909

inputs
states_0
unknown
	unknown_0
	unknown_1
identity

identity_1ИҐStatefulPartitionedCall€
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0unknown	unknown_0	unknown_1*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_371932
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

IdentityТ

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_1"
identityIdentity:output:0"!

identity_1Identity_1:output:0*E
_input_shapes4
2:€€€€€€€€€:€€€€€€€€€ :::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:€€€€€€€€€
 
_user_specified_nameinputs:QM
'
_output_shapes
:€€€€€€€€€ 
"
_user_specified_name
states/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
®У
»
gru_1_while_body_39079
gru_1_while_loop_counter"
gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
gru_1_strided_slice_1_0W
Stensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
gru_1_strided_slice_1U
Qtensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeї
#TensorArrayV2Read/TensorListGetItemTensorListGetItemStensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1§
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul®
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1®
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/ones_like:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/ones_like_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/yd
add_1AddV2gru_1_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identitye

Identity_1Identitygru_1_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"0
gru_1_strided_slice_1gru_1_strided_slice_1_0"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"®
Qtensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensorStensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
≈’
Ю
while_body_39444
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
strided_slice_1_0Q
Mtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
strided_slice_1O
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeµ
#TensorArrayV2Read/TensorListGetItemTensorListGetItemMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2сБИ21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeъ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ИЦТ23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2±В≤23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1y
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2–юл23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeъ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ељщ23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeщ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2цУ23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1£
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul©
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1©
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2М
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3М
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4М
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/y^
add_1AddV2while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identity_

Identity_1Identitywhile_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"$
strided_slice_1strided_slice_1_0"Ь
Ktensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorMtensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
Ьх
е
@__inference_gru_1_layer_call_and_return_conditional_losses_39643
inputs_0&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resource
identityИҐwhileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permЕ
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2А
gru_cell_1/ones_like/ShapeShapestrided_slice_2:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2’ћА21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeщ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2НЇ^23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ґЯЉ23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1z
gru_cell_1/ones_like_1/ShapeShapezeros:output:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2НџВ23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeъ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2…хЫ23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ХВЈ23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1С
gru_cell_1/mulMulstrided_slice_2:output:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mulЧ
gru_cell_1/mul_1Mulstrided_slice_2:output:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1Ч
gru_cell_1/mul_2Mulstrided_slice_2:output:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Щ
gru_cell_1/ReadVariableOpReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЭ
gru_cell_1/ReadVariableOp_1ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Э
gru_cell_1/ReadVariableOp_2ReadVariableOp"gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Ы
gru_cell_1/ReadVariableOp_3ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЫ
gru_cell_1/ReadVariableOp_4ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Ы
gru_cell_1/ReadVariableOp_5ReadVariableOp$gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2Н
gru_cell_1/mul_3Mulzeros:output:0gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3Н
gru_cell_1/mul_4Mulzeros:output:0gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4Н
gru_cell_1/mul_5Mulzeros:output:0gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5Я
gru_cell_1/ReadVariableOp_6ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3Я
gru_cell_1/ReadVariableOp_7ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8Я
gru_cell_1/ReadVariableOp_8ReadVariableOp$gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhЛ
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5П
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterЩ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_3_resource$gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_39444*
condR
while_cond_39443*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimet
IdentityIdentitystrided_slice_3:output:0^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2
whilewhile:^ Z
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
"
_user_specified_name
inputs/0:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
г
while_cond_38253
while_loop_counter
while_maximum_iterations
placeholder
placeholder_1
placeholder_2
less_strided_slice_1-
)while_cond_38253___redundant_placeholder0-
)while_cond_38253___redundant_placeholder1-
)while_cond_38253___redundant_placeholder2-
)while_cond_38253___redundant_placeholder3
identity
X
LessLessplaceholderless_strided_slice_1*
T0*
_output_shapes
: 2
LessK
IdentityIdentityLess:z:0*
T0
*
_output_shapes
: 2

Identity"
identityIdentity:output:0*@
_input_shapes/
-: : : : :€€€€€€€€€ : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
::

_output_shapes
::

_output_shapes
::	

_output_shapes
:
А=
ќ
@__inference_gru_1_layer_call_and_return_conditional_losses_37617

inputs
gru_cell_1_37541
gru_cell_1_37543
gru_cell_1_37545
identityИҐ"gru_cell_1/StatefulPartitionedCallҐwhileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2в
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
zeros/packed/1Г
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
zerosu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permГ
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2о
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1Е
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
TensorArrayV2/element_shape≤
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2њ
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeш
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2ь
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
strided_slice_2ƒ
"gru_cell_1/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0gru_cell_1_37541gru_cell_1_37543gru_cell_1_37545*
Tin	
2*
Tout
2*:
_output_shapes(
&:€€€€€€€€€ :€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*N
fIRG
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_371932$
"gru_cell_1/StatefulPartitionedCallП
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2
TensorArrayV2_1/element_shapeЄ
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterя
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0gru_cell_1_37541gru_cell_1_37543gru_cell_1_37545*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*
bodyR
while_body_37553*
condR
while_cond_37552*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    22
0TensorArrayV2Stack/TensorListStack/element_shapeс
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02$
"TensorArrayV2Stack/TensorListStackБ
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2Ъ
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/permЃ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeЩ
IdentityIdentitystrided_slice_3:output:0#^gru_cell_1/StatefulPartitionedCall^while*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::2H
"gru_cell_1/StatefulPartitionedCall"gru_cell_1/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ЈЧ
џ
G__inference_sequential_1_layer_call_and_return_conditional_losses_38949

inputs,
(gru_1_gru_cell_1_readvariableop_resource.
*gru_1_gru_cell_1_readvariableop_3_resource.
*gru_1_gru_cell_1_readvariableop_6_resource*
&dense_1_matmul_readvariableop_resource+
'dense_1_biasadd_readvariableop_resource
identityИҐgru_1/whileP
gru_1/ShapeShapeinputs*
T0*
_output_shapes
:2
gru_1/ShapeА
gru_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice/stackД
gru_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_1Д
gru_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice/stack_2Ж
gru_1/strided_sliceStridedSlicegru_1/Shape:output:0"gru_1/strided_slice/stack:output:0$gru_1/strided_slice/stack_1:output:0$gru_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_sliceh
gru_1/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/mul/yД
gru_1/zeros/mulMulgru_1/strided_slice:output:0gru_1/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
gru_1/zeros/mulk
gru_1/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :и2
gru_1/zeros/Less/y
gru_1/zeros/LessLessgru_1/zeros/mul:z:0gru_1/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
gru_1/zeros/Lessn
gru_1/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/zeros/packed/1Ы
gru_1/zeros/packedPackgru_1/strided_slice:output:0gru_1/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
gru_1/zeros/packedk
gru_1/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/zeros/ConstН
gru_1/zerosFillgru_1/zeros/packed:output:0gru_1/zeros/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/zerosБ
gru_1/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose/permХ
gru_1/transpose	Transposeinputsgru_1/transpose/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€2
gru_1/transposea
gru_1/Shape_1Shapegru_1/transpose:y:0*
T0*
_output_shapes
:2
gru_1/Shape_1Д
gru_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_1/stackИ
gru_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_1И
gru_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_1/stack_2Т
gru_1/strided_slice_1StridedSlicegru_1/Shape_1:output:0$gru_1/strided_slice_1/stack:output:0&gru_1/strided_slice_1/stack_1:output:0&gru_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
gru_1/strided_slice_1С
!gru_1/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2#
!gru_1/TensorArrayV2/element_shape 
gru_1/TensorArrayV2TensorListReserve*gru_1/TensorArrayV2/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2Ћ
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   2=
;gru_1/TensorArrayUnstack/TensorListFromTensor/element_shapeР
-gru_1/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorgru_1/transpose:y:0Dgru_1/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02/
-gru_1/TensorArrayUnstack/TensorListFromTensorД
gru_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_2/stackИ
gru_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_1И
gru_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_2/stack_2†
gru_1/strided_slice_2StridedSlicegru_1/transpose:y:0$gru_1/strided_slice_2/stack:output:0&gru_1/strided_slice_2/stack_1:output:0&gru_1/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€*
shrink_axis_mask2
gru_1/strided_slice_2Т
 gru_1/gru_cell_1/ones_like/ShapeShapegru_1/strided_slice_2:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/ones_like/ShapeЙ
 gru_1/gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2"
 gru_1/gru_cell_1/ones_like/Const»
gru_1/gru_cell_1/ones_likeFill)gru_1/gru_cell_1/ones_like/Shape:output:0)gru_1/gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/ones_likeЕ
gru_1/gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2 
gru_1/gru_cell_1/dropout/Const√
gru_1/gru_cell_1/dropout/MulMul#gru_1/gru_cell_1/ones_like:output:0'gru_1/gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/dropout/MulУ
gru_1/gru_cell_1/dropout/ShapeShape#gru_1/gru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2 
gru_1/gru_cell_1/dropout/ShapeЖ
5gru_1/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform'gru_1/gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ьЖФ27
5gru_1/gru_cell_1/dropout/random_uniform/RandomUniformЧ
'gru_1/gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2)
'gru_1/gru_cell_1/dropout/GreaterEqual/yВ
%gru_1/gru_cell_1/dropout/GreaterEqualGreaterEqual>gru_1/gru_cell_1/dropout/random_uniform/RandomUniform:output:00gru_1/gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2'
%gru_1/gru_cell_1/dropout/GreaterEqual≤
gru_1/gru_cell_1/dropout/CastCast)gru_1/gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/dropout/CastЊ
gru_1/gru_cell_1/dropout/Mul_1Mul gru_1/gru_cell_1/dropout/Mul:z:0!gru_1/gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2 
gru_1/gru_cell_1/dropout/Mul_1Й
 gru_1/gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2"
 gru_1/gru_cell_1/dropout_1/Const…
gru_1/gru_cell_1/dropout_1/MulMul#gru_1/gru_cell_1/ones_like:output:0)gru_1/gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2 
gru_1/gru_cell_1/dropout_1/MulЧ
 gru_1/gru_cell_1/dropout_1/ShapeShape#gru_1/gru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/dropout_1/ShapeМ
7gru_1/gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform)gru_1/gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2н‘»29
7gru_1/gru_cell_1/dropout_1/random_uniform/RandomUniformЫ
)gru_1/gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2+
)gru_1/gru_cell_1/dropout_1/GreaterEqual/yК
'gru_1/gru_cell_1/dropout_1/GreaterEqualGreaterEqual@gru_1/gru_cell_1/dropout_1/random_uniform/RandomUniform:output:02gru_1/gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2)
'gru_1/gru_cell_1/dropout_1/GreaterEqualЄ
gru_1/gru_cell_1/dropout_1/CastCast+gru_1/gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2!
gru_1/gru_cell_1/dropout_1/Cast∆
 gru_1/gru_cell_1/dropout_1/Mul_1Mul"gru_1/gru_cell_1/dropout_1/Mul:z:0#gru_1/gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2"
 gru_1/gru_cell_1/dropout_1/Mul_1Й
 gru_1/gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2"
 gru_1/gru_cell_1/dropout_2/Const…
gru_1/gru_cell_1/dropout_2/MulMul#gru_1/gru_cell_1/ones_like:output:0)gru_1/gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2 
gru_1/gru_cell_1/dropout_2/MulЧ
 gru_1/gru_cell_1/dropout_2/ShapeShape#gru_1/gru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/dropout_2/ShapeМ
7gru_1/gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform)gru_1/gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ƒЁН29
7gru_1/gru_cell_1/dropout_2/random_uniform/RandomUniformЫ
)gru_1/gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2+
)gru_1/gru_cell_1/dropout_2/GreaterEqual/yК
'gru_1/gru_cell_1/dropout_2/GreaterEqualGreaterEqual@gru_1/gru_cell_1/dropout_2/random_uniform/RandomUniform:output:02gru_1/gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2)
'gru_1/gru_cell_1/dropout_2/GreaterEqualЄ
gru_1/gru_cell_1/dropout_2/CastCast+gru_1/gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2!
gru_1/gru_cell_1/dropout_2/Cast∆
 gru_1/gru_cell_1/dropout_2/Mul_1Mul"gru_1/gru_cell_1/dropout_2/Mul:z:0#gru_1/gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2"
 gru_1/gru_cell_1/dropout_2/Mul_1М
"gru_1/gru_cell_1/ones_like_1/ShapeShapegru_1/zeros:output:0*
T0*
_output_shapes
:2$
"gru_1/gru_cell_1/ones_like_1/ShapeН
"gru_1/gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_1/gru_cell_1/ones_like_1/Const–
gru_1/gru_cell_1/ones_like_1Fill+gru_1/gru_cell_1/ones_like_1/Shape:output:0+gru_1/gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/ones_like_1Й
 gru_1/gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2"
 gru_1/gru_cell_1/dropout_3/ConstЋ
gru_1/gru_cell_1/dropout_3/MulMul%gru_1/gru_cell_1/ones_like_1:output:0)gru_1/gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/gru_cell_1/dropout_3/MulЩ
 gru_1/gru_cell_1/dropout_3/ShapeShape%gru_1/gru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/dropout_3/ShapeЛ
7gru_1/gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform)gru_1/gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2∆»b29
7gru_1/gru_cell_1/dropout_3/random_uniform/RandomUniformЫ
)gru_1/gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2+
)gru_1/gru_cell_1/dropout_3/GreaterEqual/yК
'gru_1/gru_cell_1/dropout_3/GreaterEqualGreaterEqual@gru_1/gru_cell_1/dropout_3/random_uniform/RandomUniform:output:02gru_1/gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'gru_1/gru_cell_1/dropout_3/GreaterEqualЄ
gru_1/gru_cell_1/dropout_3/CastCast+gru_1/gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2!
gru_1/gru_cell_1/dropout_3/Cast∆
 gru_1/gru_cell_1/dropout_3/Mul_1Mul"gru_1/gru_cell_1/dropout_3/Mul:z:0#gru_1/gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/gru_cell_1/dropout_3/Mul_1Й
 gru_1/gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2"
 gru_1/gru_cell_1/dropout_4/ConstЋ
gru_1/gru_cell_1/dropout_4/MulMul%gru_1/gru_cell_1/ones_like_1:output:0)gru_1/gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/gru_cell_1/dropout_4/MulЩ
 gru_1/gru_cell_1/dropout_4/ShapeShape%gru_1/gru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/dropout_4/ShapeЛ
7gru_1/gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform)gru_1/gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2®шr29
7gru_1/gru_cell_1/dropout_4/random_uniform/RandomUniformЫ
)gru_1/gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2+
)gru_1/gru_cell_1/dropout_4/GreaterEqual/yК
'gru_1/gru_cell_1/dropout_4/GreaterEqualGreaterEqual@gru_1/gru_cell_1/dropout_4/random_uniform/RandomUniform:output:02gru_1/gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'gru_1/gru_cell_1/dropout_4/GreaterEqualЄ
gru_1/gru_cell_1/dropout_4/CastCast+gru_1/gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2!
gru_1/gru_cell_1/dropout_4/Cast∆
 gru_1/gru_cell_1/dropout_4/Mul_1Mul"gru_1/gru_cell_1/dropout_4/Mul:z:0#gru_1/gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/gru_cell_1/dropout_4/Mul_1Й
 gru_1/gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2"
 gru_1/gru_cell_1/dropout_5/ConstЋ
gru_1/gru_cell_1/dropout_5/MulMul%gru_1/gru_cell_1/ones_like_1:output:0)gru_1/gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/gru_cell_1/dropout_5/MulЩ
 gru_1/gru_cell_1/dropout_5/ShapeShape%gru_1/gru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2"
 gru_1/gru_cell_1/dropout_5/ShapeЛ
7gru_1/gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform)gru_1/gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2ў§L29
7gru_1/gru_cell_1/dropout_5/random_uniform/RandomUniformЫ
)gru_1/gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2+
)gru_1/gru_cell_1/dropout_5/GreaterEqual/yК
'gru_1/gru_cell_1/dropout_5/GreaterEqualGreaterEqual@gru_1/gru_cell_1/dropout_5/random_uniform/RandomUniform:output:02gru_1/gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2)
'gru_1/gru_cell_1/dropout_5/GreaterEqualЄ
gru_1/gru_cell_1/dropout_5/CastCast+gru_1/gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2!
gru_1/gru_cell_1/dropout_5/Cast∆
 gru_1/gru_cell_1/dropout_5/Mul_1Mul"gru_1/gru_cell_1/dropout_5/Mul:z:0#gru_1/gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/gru_cell_1/dropout_5/Mul_1©
gru_1/gru_cell_1/mulMulgru_1/strided_slice_2:output:0"gru_1/gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mulѓ
gru_1/gru_cell_1/mul_1Mulgru_1/strided_slice_2:output:0$gru_1/gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mul_1ѓ
gru_1/gru_cell_1/mul_2Mulgru_1/strided_slice_2:output:0$gru_1/gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_1/gru_cell_1/mul_2Ђ
gru_1/gru_cell_1/ReadVariableOpReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02!
gru_1/gru_cell_1/ReadVariableOpЭ
$gru_1/gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2&
$gru_1/gru_cell_1/strided_slice/stack°
&gru_1/gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice/stack_1°
&gru_1/gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2(
&gru_1/gru_cell_1/strided_slice/stack_2в
gru_1/gru_cell_1/strided_sliceStridedSlice'gru_1/gru_cell_1/ReadVariableOp:value:0-gru_1/gru_cell_1/strided_slice/stack:output:0/gru_1/gru_cell_1/strided_slice/stack_1:output:0/gru_1/gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2 
gru_1/gru_cell_1/strided_slice±
gru_1/gru_cell_1/MatMulMatMulgru_1/gru_cell_1/mul:z:0'gru_1/gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMulѓ
!gru_1/gru_cell_1/ReadVariableOp_1ReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_1°
&gru_1/gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_1/stack•
(gru_1/gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2*
(gru_1/gru_cell_1/strided_slice_1/stack_1•
(gru_1/gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_1/stack_2о
 gru_1/gru_cell_1/strided_slice_1StridedSlice)gru_1/gru_cell_1/ReadVariableOp_1:value:0/gru_1/gru_cell_1/strided_slice_1/stack:output:01gru_1/gru_cell_1/strided_slice_1/stack_1:output:01gru_1/gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_1є
gru_1/gru_cell_1/MatMul_1MatMulgru_1/gru_cell_1/mul_1:z:0)gru_1/gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_1ѓ
!gru_1/gru_cell_1/ReadVariableOp_2ReadVariableOp(gru_1_gru_cell_1_readvariableop_resource*
_output_shapes

:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_2°
&gru_1/gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2(
&gru_1/gru_cell_1/strided_slice_2/stack•
(gru_1/gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_2/stack_1•
(gru_1/gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_2/stack_2о
 gru_1/gru_cell_1/strided_slice_2StridedSlice)gru_1/gru_cell_1/ReadVariableOp_2:value:0/gru_1/gru_cell_1/strided_slice_2/stack:output:01gru_1/gru_cell_1/strided_slice_2/stack_1:output:01gru_1/gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_2є
gru_1/gru_cell_1/MatMul_2MatMulgru_1/gru_cell_1/mul_2:z:0)gru_1/gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_2≠
!gru_1/gru_cell_1/ReadVariableOp_3ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_3Ъ
&gru_1/gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&gru_1/gru_cell_1/strided_slice_3/stackЮ
(gru_1/gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2*
(gru_1/gru_cell_1/strided_slice_3/stack_1Ю
(gru_1/gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_3/stack_2Џ
 gru_1/gru_cell_1/strided_slice_3StridedSlice)gru_1/gru_cell_1/ReadVariableOp_3:value:0/gru_1/gru_cell_1/strided_slice_3/stack:output:01gru_1/gru_cell_1/strided_slice_3/stack_1:output:01gru_1/gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2"
 gru_1/gru_cell_1/strided_slice_3њ
gru_1/gru_cell_1/BiasAddBiasAdd!gru_1/gru_cell_1/MatMul:product:0)gru_1/gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd≠
!gru_1/gru_cell_1/ReadVariableOp_4ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_4Ъ
&gru_1/gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2(
&gru_1/gru_cell_1/strided_slice_4/stackЮ
(gru_1/gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2*
(gru_1/gru_cell_1/strided_slice_4/stack_1Ю
(gru_1/gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_4/stack_2»
 gru_1/gru_cell_1/strided_slice_4StridedSlice)gru_1/gru_cell_1/ReadVariableOp_4:value:0/gru_1/gru_cell_1/strided_slice_4/stack:output:01gru_1/gru_cell_1/strided_slice_4/stack_1:output:01gru_1/gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2"
 gru_1/gru_cell_1/strided_slice_4≈
gru_1/gru_cell_1/BiasAdd_1BiasAdd#gru_1/gru_cell_1/MatMul_1:product:0)gru_1/gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd_1≠
!gru_1/gru_cell_1/ReadVariableOp_5ReadVariableOp*gru_1_gru_cell_1_readvariableop_3_resource*
_output_shapes
:`*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_5Ъ
&gru_1/gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2(
&gru_1/gru_cell_1/strided_slice_5/stackЮ
(gru_1/gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2*
(gru_1/gru_cell_1/strided_slice_5/stack_1Ю
(gru_1/gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2*
(gru_1/gru_cell_1/strided_slice_5/stack_2Ў
 gru_1/gru_cell_1/strided_slice_5StridedSlice)gru_1/gru_cell_1/ReadVariableOp_5:value:0/gru_1/gru_cell_1/strided_slice_5/stack:output:01gru_1/gru_cell_1/strided_slice_5/stack_1:output:01gru_1/gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2"
 gru_1/gru_cell_1/strided_slice_5≈
gru_1/gru_cell_1/BiasAdd_2BiasAdd#gru_1/gru_cell_1/MatMul_2:product:0)gru_1/gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/BiasAdd_2•
gru_1/gru_cell_1/mul_3Mulgru_1/zeros:output:0$gru_1/gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_3•
gru_1/gru_cell_1/mul_4Mulgru_1/zeros:output:0$gru_1/gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_4•
gru_1/gru_cell_1/mul_5Mulgru_1/zeros:output:0$gru_1/gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_5±
!gru_1/gru_cell_1/ReadVariableOp_6ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_6°
&gru_1/gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_6/stack•
(gru_1/gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_6/stack_1•
(gru_1/gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_6/stack_2о
 gru_1/gru_cell_1/strided_slice_6StridedSlice)gru_1/gru_cell_1/ReadVariableOp_6:value:0/gru_1/gru_cell_1/strided_slice_6/stack:output:01gru_1/gru_cell_1/strided_slice_6/stack_1:output:01gru_1/gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_6є
gru_1/gru_cell_1/MatMul_3MatMulgru_1/gru_cell_1/mul_3:z:0)gru_1/gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_3±
!gru_1/gru_cell_1/ReadVariableOp_7ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_7°
&gru_1/gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2(
&gru_1/gru_cell_1/strided_slice_7/stack•
(gru_1/gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2*
(gru_1/gru_cell_1/strided_slice_7/stack_1•
(gru_1/gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_7/stack_2о
 gru_1/gru_cell_1/strided_slice_7StridedSlice)gru_1/gru_cell_1/ReadVariableOp_7:value:0/gru_1/gru_cell_1/strided_slice_7/stack:output:01gru_1/gru_cell_1/strided_slice_7/stack_1:output:01gru_1/gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_7є
gru_1/gru_cell_1/MatMul_4MatMulgru_1/gru_cell_1/mul_4:z:0)gru_1/gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_4ѓ
gru_1/gru_cell_1/addAddV2!gru_1/gru_cell_1/BiasAdd:output:0#gru_1/gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/addu
gru_1/gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_1/gru_cell_1/Consty
gru_1/gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_1/gru_cell_1/Const_1§
gru_1/gru_cell_1/Mul_6Mulgru_1/gru_cell_1/add:z:0gru_1/gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Mul_6®
gru_1/gru_cell_1/Add_1Addgru_1/gru_cell_1/Mul_6:z:0!gru_1/gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Add_1Щ
(gru_1/gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2*
(gru_1/gru_cell_1/clip_by_value/Minimum/y№
&gru_1/gru_cell_1/clip_by_value/MinimumMinimumgru_1/gru_cell_1/Add_1:z:01gru_1/gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2(
&gru_1/gru_cell_1/clip_by_value/MinimumЙ
 gru_1/gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2"
 gru_1/gru_cell_1/clip_by_value/y‘
gru_1/gru_cell_1/clip_by_valueMaximum*gru_1/gru_cell_1/clip_by_value/Minimum:z:0)gru_1/gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2 
gru_1/gru_cell_1/clip_by_valueµ
gru_1/gru_cell_1/add_2AddV2#gru_1/gru_cell_1/BiasAdd_1:output:0#gru_1/gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_2y
gru_1/gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_1/gru_cell_1/Const_2y
gru_1/gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_1/gru_cell_1/Const_3®
gru_1/gru_cell_1/Mul_7Mulgru_1/gru_cell_1/add_2:z:0!gru_1/gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Mul_7®
gru_1/gru_cell_1/Add_3Addgru_1/gru_cell_1/Mul_7:z:0!gru_1/gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Add_3Э
*gru_1/gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2,
*gru_1/gru_cell_1/clip_by_value_1/Minimum/yв
(gru_1/gru_cell_1/clip_by_value_1/MinimumMinimumgru_1/gru_cell_1/Add_3:z:03gru_1/gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2*
(gru_1/gru_cell_1/clip_by_value_1/MinimumН
"gru_1/gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"gru_1/gru_cell_1/clip_by_value_1/y№
 gru_1/gru_cell_1/clip_by_value_1Maximum,gru_1/gru_cell_1/clip_by_value_1/Minimum:z:0+gru_1/gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_1/gru_cell_1/clip_by_value_1Ђ
gru_1/gru_cell_1/mul_8Mul$gru_1/gru_cell_1/clip_by_value_1:z:0gru_1/gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_8±
!gru_1/gru_cell_1/ReadVariableOp_8ReadVariableOp*gru_1_gru_cell_1_readvariableop_6_resource*
_output_shapes

: `*
dtype02#
!gru_1/gru_cell_1/ReadVariableOp_8°
&gru_1/gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2(
&gru_1/gru_cell_1/strided_slice_8/stack•
(gru_1/gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2*
(gru_1/gru_cell_1/strided_slice_8/stack_1•
(gru_1/gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2*
(gru_1/gru_cell_1/strided_slice_8/stack_2о
 gru_1/gru_cell_1/strided_slice_8StridedSlice)gru_1/gru_cell_1/ReadVariableOp_8:value:0/gru_1/gru_cell_1/strided_slice_8/stack:output:01gru_1/gru_cell_1/strided_slice_8/stack_1:output:01gru_1/gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2"
 gru_1/gru_cell_1/strided_slice_8є
gru_1/gru_cell_1/MatMul_5MatMulgru_1/gru_cell_1/mul_8:z:0)gru_1/gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/MatMul_5µ
gru_1/gru_cell_1/add_4AddV2#gru_1/gru_cell_1/BiasAdd_2:output:0#gru_1/gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_4Д
gru_1/gru_cell_1/TanhTanhgru_1/gru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/Tanh£
gru_1/gru_cell_1/mul_9Mul"gru_1/gru_cell_1/clip_by_value:z:0gru_1/zeros:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_9u
gru_1/gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_1/gru_cell_1/sub/x™
gru_1/gru_cell_1/subSubgru_1/gru_cell_1/sub/x:output:0"gru_1/gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/sub†
gru_1/gru_cell_1/mul_10Mulgru_1/gru_cell_1/sub:z:0gru_1/gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/mul_10§
gru_1/gru_cell_1/add_5AddV2gru_1/gru_cell_1/mul_9:z:0gru_1/gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_1/gru_cell_1/add_5Ы
#gru_1/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    2%
#gru_1/TensorArrayV2_1/element_shape–
gru_1/TensorArrayV2_1TensorListReserve,gru_1/TensorArrayV2_1/element_shape:output:0gru_1/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
gru_1/TensorArrayV2_1Z

gru_1/timeConst*
_output_shapes
: *
dtype0*
value	B : 2

gru_1/timeЛ
gru_1/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
€€€€€€€€€2 
gru_1/while/maximum_iterationsv
gru_1/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
gru_1/while/loop_counterн
gru_1/whileWhile!gru_1/while/loop_counter:output:0'gru_1/while/maximum_iterations:output:0gru_1/time:output:0gru_1/TensorArrayV2_1:handle:0gru_1/zeros:output:0gru_1/strided_slice_1:output:0=gru_1/TensorArrayUnstack/TensorListFromTensor:output_handle:0(gru_1_gru_cell_1_readvariableop_resource*gru_1_gru_cell_1_readvariableop_3_resource*gru_1_gru_cell_1_readvariableop_6_resource*
T
2
*
_lower_using_switch_merge(*
_num_original_outputs
*9
_output_shapes'
%: : : : :€€€€€€€€€ : : : : : *%
_read_only_resource_inputs
	*"
bodyR
gru_1_while_body_38744*"
condR
gru_1_while_cond_38743*8
output_shapes'
%: : : : :€€€€€€€€€ : : : : : *
parallel_iterations 2
gru_1/whileЅ
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€    28
6gru_1/TensorArrayV2Stack/TensorListStack/element_shapeЙ
(gru_1/TensorArrayV2Stack/TensorListStackTensorListStackgru_1/while:output:3?gru_1/TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ *
element_dtype02*
(gru_1/TensorArrayV2Stack/TensorListStackН
gru_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
€€€€€€€€€2
gru_1/strided_slice_3/stackИ
gru_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
gru_1/strided_slice_3/stack_1И
gru_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
gru_1/strided_slice_3/stack_2Њ
gru_1/strided_slice_3StridedSlice1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0$gru_1/strided_slice_3/stack:output:0&gru_1/strided_slice_3/stack_1:output:0&gru_1/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:€€€€€€€€€ *
shrink_axis_mask2
gru_1/strided_slice_3Е
gru_1/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
gru_1/transpose_1/perm∆
gru_1/transpose_1	Transpose1gru_1/TensorArrayV2Stack/TensorListStack:tensor:0gru_1/transpose_1/perm:output:0*
T0*4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€ 2
gru_1/transpose_1r
gru_1/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_1/runtime•
dense_1/MatMul/ReadVariableOpReadVariableOp&dense_1_matmul_readvariableop_resource*
_output_shapes

: *
dtype02
dense_1/MatMul/ReadVariableOp£
dense_1/MatMulMatMulgru_1/strided_slice_3:output:0%dense_1/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense_1/MatMul§
dense_1/BiasAdd/ReadVariableOpReadVariableOp'dense_1_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_1/BiasAdd/ReadVariableOp°
dense_1/BiasAddBiasAdddense_1/MatMul:product:0&dense_1/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:€€€€€€€€€2
dense_1/BiasAddz
IdentityIdentitydense_1/BiasAdd:output:0^gru_1/while*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2
gru_1/whilegru_1/while:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
≥
Ж
G__inference_sequential_1_layer_call_and_return_conditional_losses_38462
gru_1_input
gru_1_38428
gru_1_38430
gru_1_38432
dense_1_38456
dense_1_38458
identityИҐdense_1/StatefulPartitionedCallҐgru_1/StatefulPartitionedCallф
gru_1/StatefulPartitionedCallStatefulPartitionedCallgru_1_inputgru_1_38428gru_1_38430gru_1_38432*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_381242
gru_1/StatefulPartitionedCallК
dense_1/StatefulPartitionedCallStatefulPartitionedCall&gru_1/StatefulPartitionedCall:output:0dense_1_38456dense_1_38458*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€*$
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*K
fFRD
B__inference_dense_1_layer_call_and_return_conditional_losses_384452!
dense_1/StatefulPartitionedCallЊ
IdentityIdentity(dense_1/StatefulPartitionedCall:output:0 ^dense_1/StatefulPartitionedCall^gru_1/StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€2

Identity"
identityIdentity:output:0*G
_input_shapes6
4:€€€€€€€€€€€€€€€€€€:::::2B
dense_1/StatefulPartitionedCalldense_1/StatefulPartitionedCall2>
gru_1/StatefulPartitionedCallgru_1/StatefulPartitionedCall:a ]
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
%
_user_specified_namegru_1_input:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Щ÷
»
gru_1_while_body_38744
gru_1_while_loop_counter"
gru_1_while_maximum_iterations
placeholder
placeholder_1
placeholder_2
gru_1_strided_slice_1_0W
Stensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0(
$gru_cell_1_readvariableop_resource_0*
&gru_cell_1_readvariableop_3_resource_0*
&gru_cell_1_readvariableop_6_resource_0
identity

identity_1

identity_2

identity_3

identity_4
gru_1_strided_slice_1U
Qtensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor&
"gru_cell_1_readvariableop_resource(
$gru_cell_1_readvariableop_3_resource(
$gru_cell_1_readvariableop_6_resourceИЈ
1TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"€€€€   23
1TensorArrayV2Read/TensorListGetItem/element_shapeї
#TensorArrayV2Read/TensorListGetItemTensorListGetItemStensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0placeholder:TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:€€€€€€€€€*
element_dtype02%
#TensorArrayV2Read/TensorListGetItemТ
gru_cell_1/ones_like/ShapeShape*TensorArrayV2Read/TensorListGetItem:item:0*
T0*
_output_shapes
:2
gru_cell_1/ones_like/Shape}
gru_cell_1/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like/Const∞
gru_cell_1/ones_likeFill#gru_cell_1/ones_like/Shape:output:0#gru_cell_1/ones_like/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/ones_likey
gru_cell_1/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout/ConstЂ
gru_cell_1/dropout/MulMulgru_cell_1/ones_like:output:0!gru_cell_1/dropout/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/MulБ
gru_cell_1/dropout/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout/Shapeф
/gru_cell_1/dropout/random_uniform/RandomUniformRandomUniform!gru_cell_1/dropout/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ЩМЌ21
/gru_cell_1/dropout/random_uniform/RandomUniformЛ
!gru_cell_1/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2#
!gru_cell_1/dropout/GreaterEqual/yк
gru_cell_1/dropout/GreaterEqualGreaterEqual8gru_cell_1/dropout/random_uniform/RandomUniform:output:0*gru_cell_1/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2!
gru_cell_1/dropout/GreaterEqual†
gru_cell_1/dropout/CastCast#gru_cell_1/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Cast¶
gru_cell_1/dropout/Mul_1Mulgru_cell_1/dropout/Mul:z:0gru_cell_1/dropout/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout/Mul_1}
gru_cell_1/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_1/Const±
gru_cell_1/dropout_1/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/MulЕ
gru_cell_1/dropout_1/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_1/Shapeъ
1gru_cell_1/dropout_1/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_1/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2ШЗ≈23
1gru_cell_1/dropout_1/random_uniform/RandomUniformП
#gru_cell_1/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_1/GreaterEqual/yт
!gru_cell_1/dropout_1/GreaterEqualGreaterEqual:gru_cell_1/dropout_1/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_1/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_1/GreaterEqual¶
gru_cell_1/dropout_1/CastCast%gru_cell_1/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/CastЃ
gru_cell_1/dropout_1/Mul_1Mulgru_cell_1/dropout_1/Mul:z:0gru_cell_1/dropout_1/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_1/Mul_1}
gru_cell_1/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  †?2
gru_cell_1/dropout_2/Const±
gru_cell_1/dropout_2/MulMulgru_cell_1/ones_like:output:0#gru_cell_1/dropout_2/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/MulЕ
gru_cell_1/dropout_2/ShapeShapegru_cell_1/ones_like:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_2/Shapeъ
1gru_cell_1/dropout_2/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_2/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€*
dtype0*
seed±€е)*
seed2Уц∞23
1gru_cell_1/dropout_2/random_uniform/RandomUniformП
#gru_cell_1/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2%
#gru_cell_1/dropout_2/GreaterEqual/yт
!gru_cell_1/dropout_2/GreaterEqualGreaterEqual:gru_cell_1/dropout_2/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_2/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€2#
!gru_cell_1/dropout_2/GreaterEqual¶
gru_cell_1/dropout_2/CastCast%gru_cell_1/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/CastЃ
gru_cell_1/dropout_2/Mul_1Mulgru_cell_1/dropout_2/Mul:z:0gru_cell_1/dropout_2/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/dropout_2/Mul_1y
gru_cell_1/ones_like_1/ShapeShapeplaceholder_2*
T0*
_output_shapes
:2
gru_cell_1/ones_like_1/ShapeБ
gru_cell_1/ones_like_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/ones_like_1/ConstЄ
gru_cell_1/ones_like_1Fill%gru_cell_1/ones_like_1/Shape:output:0%gru_cell_1/ones_like_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/ones_like_1}
gru_cell_1/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_3/Const≥
gru_cell_1/dropout_3/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_3/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/MulЗ
gru_cell_1/dropout_3/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_3/Shapeъ
1gru_cell_1/dropout_3/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_3/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2диі23
1gru_cell_1/dropout_3/random_uniform/RandomUniformП
#gru_cell_1/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_3/GreaterEqual/yт
!gru_cell_1/dropout_3/GreaterEqualGreaterEqual:gru_cell_1/dropout_3/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_3/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_3/GreaterEqual¶
gru_cell_1/dropout_3/CastCast%gru_cell_1/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/CastЃ
gru_cell_1/dropout_3/Mul_1Mulgru_cell_1/dropout_3/Mul:z:0gru_cell_1/dropout_3/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_3/Mul_1}
gru_cell_1/dropout_4/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_4/Const≥
gru_cell_1/dropout_4/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_4/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/MulЗ
gru_cell_1/dropout_4/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_4/Shapeщ
1gru_cell_1/dropout_4/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_4/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2„Н23
1gru_cell_1/dropout_4/random_uniform/RandomUniformП
#gru_cell_1/dropout_4/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_4/GreaterEqual/yт
!gru_cell_1/dropout_4/GreaterEqualGreaterEqual:gru_cell_1/dropout_4/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_4/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_4/GreaterEqual¶
gru_cell_1/dropout_4/CastCast%gru_cell_1/dropout_4/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/CastЃ
gru_cell_1/dropout_4/Mul_1Mulgru_cell_1/dropout_4/Mul:z:0gru_cell_1/dropout_4/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_4/Mul_1}
gru_cell_1/dropout_5/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *UU’?2
gru_cell_1/dropout_5/Const≥
gru_cell_1/dropout_5/MulMulgru_cell_1/ones_like_1:output:0#gru_cell_1/dropout_5/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/MulЗ
gru_cell_1/dropout_5/ShapeShapegru_cell_1/ones_like_1:output:0*
T0*
_output_shapes
:2
gru_cell_1/dropout_5/Shapeъ
1gru_cell_1/dropout_5/random_uniform/RandomUniformRandomUniform#gru_cell_1/dropout_5/Shape:output:0*
T0*'
_output_shapes
:€€€€€€€€€ *
dtype0*
seed±€е)*
seed2и•√23
1gru_cell_1/dropout_5/random_uniform/RandomUniformП
#gru_cell_1/dropout_5/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *Ќћћ>2%
#gru_cell_1/dropout_5/GreaterEqual/yт
!gru_cell_1/dropout_5/GreaterEqualGreaterEqual:gru_cell_1/dropout_5/random_uniform/RandomUniform:output:0,gru_cell_1/dropout_5/GreaterEqual/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2#
!gru_cell_1/dropout_5/GreaterEqual¶
gru_cell_1/dropout_5/CastCast%gru_cell_1/dropout_5/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/CastЃ
gru_cell_1/dropout_5/Mul_1Mulgru_cell_1/dropout_5/Mul:z:0gru_cell_1/dropout_5/Cast:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/dropout_5/Mul_1£
gru_cell_1/mulMul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul©
gru_cell_1/mul_1Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_1/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_1©
gru_cell_1/mul_2Mul*TensorArrayV2Read/TensorListGetItem:item:0gru_cell_1/dropout_2/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€2
gru_cell_1/mul_2Ы
gru_cell_1/ReadVariableOpReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOpС
gru_cell_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2 
gru_cell_1/strided_slice/stackХ
 gru_cell_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice/stack_1Х
 gru_cell_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2"
 gru_cell_1/strided_slice/stack_2Њ
gru_cell_1/strided_sliceStridedSlice!gru_cell_1/ReadVariableOp:value:0'gru_cell_1/strided_slice/stack:output:0)gru_cell_1/strided_slice/stack_1:output:0)gru_cell_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_sliceЩ
gru_cell_1/MatMulMatMulgru_cell_1/mul:z:0!gru_cell_1/strided_slice:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMulЯ
gru_cell_1/ReadVariableOp_1ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_1Х
 gru_cell_1/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_1/stackЩ
"gru_cell_1/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_1/stack_1Щ
"gru_cell_1/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_1/stack_2 
gru_cell_1/strided_slice_1StridedSlice#gru_cell_1/ReadVariableOp_1:value:0)gru_cell_1/strided_slice_1/stack:output:0+gru_cell_1/strided_slice_1/stack_1:output:0+gru_cell_1/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_1°
gru_cell_1/MatMul_1MatMulgru_cell_1/mul_1:z:0#gru_cell_1/strided_slice_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_1Я
gru_cell_1/ReadVariableOp_2ReadVariableOp$gru_cell_1_readvariableop_resource_0*
_output_shapes

:`*
dtype02
gru_cell_1/ReadVariableOp_2Х
 gru_cell_1/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_2/stackЩ
"gru_cell_1/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_2/stack_1Щ
"gru_cell_1/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_2/stack_2 
gru_cell_1/strided_slice_2StridedSlice#gru_cell_1/ReadVariableOp_2:value:0)gru_cell_1/strided_slice_2/stack:output:0+gru_cell_1/strided_slice_2/stack_1:output:0+gru_cell_1/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

: *

begin_mask*
end_mask2
gru_cell_1/strided_slice_2°
gru_cell_1/MatMul_2MatMulgru_cell_1/mul_2:z:0#gru_cell_1/strided_slice_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_2Э
gru_cell_1/ReadVariableOp_3ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_3О
 gru_cell_1/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_3/stackТ
"gru_cell_1/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_3/stack_1Т
"gru_cell_1/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_3/stack_2ґ
gru_cell_1/strided_slice_3StridedSlice#gru_cell_1/ReadVariableOp_3:value:0)gru_cell_1/strided_slice_3/stack:output:0+gru_cell_1/strided_slice_3/stack_1:output:0+gru_cell_1/strided_slice_3/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2
gru_cell_1/strided_slice_3І
gru_cell_1/BiasAddBiasAddgru_cell_1/MatMul:product:0#gru_cell_1/strided_slice_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAddЭ
gru_cell_1/ReadVariableOp_4ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_4О
 gru_cell_1/strided_slice_4/stackConst*
_output_shapes
:*
dtype0*
valueB: 2"
 gru_cell_1/strided_slice_4/stackТ
"gru_cell_1/strided_slice_4/stack_1Const*
_output_shapes
:*
dtype0*
valueB:@2$
"gru_cell_1/strided_slice_4/stack_1Т
"gru_cell_1/strided_slice_4/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_4/stack_2§
gru_cell_1/strided_slice_4StridedSlice#gru_cell_1/ReadVariableOp_4:value:0)gru_cell_1/strided_slice_4/stack:output:0+gru_cell_1/strided_slice_4/stack_1:output:0+gru_cell_1/strided_slice_4/stack_2:output:0*
Index0*
T0*
_output_shapes
: 2
gru_cell_1/strided_slice_4≠
gru_cell_1/BiasAdd_1BiasAddgru_cell_1/MatMul_1:product:0#gru_cell_1/strided_slice_4:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_1Э
gru_cell_1/ReadVariableOp_5ReadVariableOp&gru_cell_1_readvariableop_3_resource_0*
_output_shapes
:`*
dtype02
gru_cell_1/ReadVariableOp_5О
 gru_cell_1/strided_slice_5/stackConst*
_output_shapes
:*
dtype0*
valueB:@2"
 gru_cell_1/strided_slice_5/stackТ
"gru_cell_1/strided_slice_5/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2$
"gru_cell_1/strided_slice_5/stack_1Т
"gru_cell_1/strided_slice_5/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2$
"gru_cell_1/strided_slice_5/stack_2і
gru_cell_1/strided_slice_5StridedSlice#gru_cell_1/ReadVariableOp_5:value:0)gru_cell_1/strided_slice_5/stack:output:0+gru_cell_1/strided_slice_5/stack_1:output:0+gru_cell_1/strided_slice_5/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2
gru_cell_1/strided_slice_5≠
gru_cell_1/BiasAdd_2BiasAddgru_cell_1/MatMul_2:product:0#gru_cell_1/strided_slice_5:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/BiasAdd_2М
gru_cell_1/mul_3Mulplaceholder_2gru_cell_1/dropout_3/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_3М
gru_cell_1/mul_4Mulplaceholder_2gru_cell_1/dropout_4/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_4М
gru_cell_1/mul_5Mulplaceholder_2gru_cell_1/dropout_5/Mul_1:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_5°
gru_cell_1/ReadVariableOp_6ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_6Х
 gru_cell_1/strided_slice_6/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_6/stackЩ
"gru_cell_1/strided_slice_6/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_6/stack_1Щ
"gru_cell_1/strided_slice_6/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_6/stack_2 
gru_cell_1/strided_slice_6StridedSlice#gru_cell_1/ReadVariableOp_6:value:0)gru_cell_1/strided_slice_6/stack:output:0+gru_cell_1/strided_slice_6/stack_1:output:0+gru_cell_1/strided_slice_6/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_6°
gru_cell_1/MatMul_3MatMulgru_cell_1/mul_3:z:0#gru_cell_1/strided_slice_6:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_3°
gru_cell_1/ReadVariableOp_7ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_7Х
 gru_cell_1/strided_slice_7/stackConst*
_output_shapes
:*
dtype0*
valueB"        2"
 gru_cell_1/strided_slice_7/stackЩ
"gru_cell_1/strided_slice_7/stack_1Const*
_output_shapes
:*
dtype0*
valueB"    @   2$
"gru_cell_1/strided_slice_7/stack_1Щ
"gru_cell_1/strided_slice_7/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_7/stack_2 
gru_cell_1/strided_slice_7StridedSlice#gru_cell_1/ReadVariableOp_7:value:0)gru_cell_1/strided_slice_7/stack:output:0+gru_cell_1/strided_slice_7/stack_1:output:0+gru_cell_1/strided_slice_7/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_7°
gru_cell_1/MatMul_4MatMulgru_cell_1/mul_4:z:0#gru_cell_1/strided_slice_7:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_4Ч
gru_cell_1/addAddV2gru_cell_1/BiasAdd:output:0gru_cell_1/MatMul_3:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/addi
gru_cell_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Constm
gru_cell_1/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_1М
gru_cell_1/Mul_6Mulgru_cell_1/add:z:0gru_cell_1/Const:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_6Р
gru_cell_1/Add_1Addgru_cell_1/Mul_6:z:0gru_cell_1/Const_1:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_1Н
"gru_cell_1/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2$
"gru_cell_1/clip_by_value/Minimum/yƒ
 gru_cell_1/clip_by_value/MinimumMinimumgru_cell_1/Add_1:z:0+gru_cell_1/clip_by_value/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2"
 gru_cell_1/clip_by_value/Minimum}
gru_cell_1/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value/yЉ
gru_cell_1/clip_by_valueMaximum$gru_cell_1/clip_by_value/Minimum:z:0#gru_cell_1/clip_by_value/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_valueЭ
gru_cell_1/add_2AddV2gru_cell_1/BiasAdd_1:output:0gru_cell_1/MatMul_4:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_2m
gru_cell_1/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ЌћL>2
gru_cell_1/Const_2m
gru_cell_1/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
gru_cell_1/Const_3Р
gru_cell_1/Mul_7Mulgru_cell_1/add_2:z:0gru_cell_1/Const_2:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Mul_7Р
gru_cell_1/Add_3Addgru_cell_1/Mul_7:z:0gru_cell_1/Const_3:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/Add_3С
$gru_cell_1/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2&
$gru_cell_1/clip_by_value_1/Minimum/y 
"gru_cell_1/clip_by_value_1/MinimumMinimumgru_cell_1/Add_3:z:0-gru_cell_1/clip_by_value_1/Minimum/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2$
"gru_cell_1/clip_by_value_1/MinimumБ
gru_cell_1/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
gru_cell_1/clip_by_value_1/yƒ
gru_cell_1/clip_by_value_1Maximum&gru_cell_1/clip_by_value_1/Minimum:z:0%gru_cell_1/clip_by_value_1/y:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/clip_by_value_1У
gru_cell_1/mul_8Mulgru_cell_1/clip_by_value_1:z:0gru_cell_1/mul_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_8°
gru_cell_1/ReadVariableOp_8ReadVariableOp&gru_cell_1_readvariableop_6_resource_0*
_output_shapes

: `*
dtype02
gru_cell_1/ReadVariableOp_8Х
 gru_cell_1/strided_slice_8/stackConst*
_output_shapes
:*
dtype0*
valueB"    @   2"
 gru_cell_1/strided_slice_8/stackЩ
"gru_cell_1/strided_slice_8/stack_1Const*
_output_shapes
:*
dtype0*
valueB"        2$
"gru_cell_1/strided_slice_8/stack_1Щ
"gru_cell_1/strided_slice_8/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2$
"gru_cell_1/strided_slice_8/stack_2 
gru_cell_1/strided_slice_8StridedSlice#gru_cell_1/ReadVariableOp_8:value:0)gru_cell_1/strided_slice_8/stack:output:0+gru_cell_1/strided_slice_8/stack_1:output:0+gru_cell_1/strided_slice_8/stack_2:output:0*
Index0*
T0*
_output_shapes

:  *

begin_mask*
end_mask2
gru_cell_1/strided_slice_8°
gru_cell_1/MatMul_5MatMulgru_cell_1/mul_8:z:0#gru_cell_1/strided_slice_8:output:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/MatMul_5Э
gru_cell_1/add_4AddV2gru_cell_1/BiasAdd_2:output:0gru_cell_1/MatMul_5:product:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_4r
gru_cell_1/TanhTanhgru_cell_1/add_4:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/TanhК
gru_cell_1/mul_9Mulgru_cell_1/clip_by_value:z:0placeholder_2*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_9i
gru_cell_1/sub/xConst*
_output_shapes
: *
dtype0*
valueB
 *  А?2
gru_cell_1/sub/xТ
gru_cell_1/subSubgru_cell_1/sub/x:output:0gru_cell_1/clip_by_value:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/subИ
gru_cell_1/mul_10Mulgru_cell_1/sub:z:0gru_cell_1/Tanh:y:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/mul_10М
gru_cell_1/add_5AddV2gru_cell_1/mul_9:z:0gru_cell_1/mul_10:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2
gru_cell_1/add_5ј
$TensorArrayV2Write/TensorListSetItemTensorListSetItemplaceholder_1placeholdergru_cell_1/add_5:z:0*
_output_shapes
: *
element_dtype02&
$TensorArrayV2Write/TensorListSetItemP
add/yConst*
_output_shapes
: *
dtype0*
value	B :2
add/yQ
addAddV2placeholderadd/y:output:0*
T0*
_output_shapes
: 2
addT
add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2	
add_1/yd
add_1AddV2gru_1_while_loop_counteradd_1/y:output:0*
T0*
_output_shapes
: 2
add_1L
IdentityIdentity	add_1:z:0*
T0*
_output_shapes
: 2

Identitye

Identity_1Identitygru_1_while_maximum_iterations*
T0*
_output_shapes
: 2

Identity_1N

Identity_2Identityadd:z:0*
T0*
_output_shapes
: 2

Identity_2{

Identity_3Identity4TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: 2

Identity_3l

Identity_4Identitygru_cell_1/add_5:z:0*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity_4"0
gru_1_strided_slice_1gru_1_strided_slice_1_0"N
$gru_cell_1_readvariableop_3_resource&gru_cell_1_readvariableop_3_resource_0"N
$gru_cell_1_readvariableop_6_resource&gru_cell_1_readvariableop_6_resource_0"J
"gru_cell_1_readvariableop_resource$gru_cell_1_readvariableop_resource_0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"®
Qtensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensorStensorarrayv2read_tensorlistgetitem_gru_1_tensorarrayunstack_tensorlistfromtensor_0*>
_input_shapes-
+: : : : :€€€€€€€€€ : : :::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:€€€€€€€€€ :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :	

_output_shapes
: 
«
Й
%__inference_gru_1_layer_call_fn_40626

inputs
unknown
	unknown_0
	unknown_1
identityИҐStatefulPartitionedCallџ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_384052
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
«
Й
%__inference_gru_1_layer_call_fn_40615

inputs
unknown
	unknown_0
	unknown_1
identityИҐStatefulPartitionedCallџ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*'
_output_shapes
:€€€€€€€€€ *%
_read_only_resource_inputs
**
config_proto

CPU

GPU 2J 8*I
fDRB
@__inference_gru_1_layer_call_and_return_conditional_losses_381242
StatefulPartitionedCallО
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:€€€€€€€€€ 2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:€€€€€€€€€€€€€€€€€€:::22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :€€€€€€€€€€€€€€€€€€
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: "ѓL
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*њ
serving_defaultЂ
P
gru_1_inputA
serving_default_gru_1_input:0€€€€€€€€€€€€€€€€€€;
dense_10
StatefulPartitionedCall:0€€€€€€€€€tensorflow/serving/predict:ѕУ
У 
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api

signatures
@_default_save_signature
A__call__
*B&call_and_return_all_conditional_losses"ъ
_tf_keras_sequentialџ{"class_name": "Sequential", "name": "sequential_1", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "config": {"name": "sequential_1", "layers": [{"class_name": "GRU", "config": {"name": "gru_1", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.2, "recurrent_dropout": 0.4, "implementation": 1, "reset_after": false}}, {"class_name": "Dense", "config": {"name": "dense_1", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}, "is_graph_network": true, "keras_version": "2.3.0-tf", "backend": "tensorflow", "model_config": {"class_name": "Sequential", "config": {"name": "sequential_1", "layers": [{"class_name": "GRU", "config": {"name": "gru_1", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.2, "recurrent_dropout": 0.4, "implementation": 1, "reset_after": false}}, {"class_name": "Dense", "config": {"name": "dense_1", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}}, "training_config": {"loss": "mae", "metrics": null, "weighted_metrics": null, "loss_weights": null, "sample_weight_mode": null, "optimizer_config": {"class_name": "RMSprop", "config": {"name": "RMSprop", "learning_rate": 0.0010000000474974513, "decay": 0.0, "rho": 0.8999999761581421, "momentum": 0.0, "epsilon": 1e-07, "centered": false}}}}
Х
	cell


state_spec
	variables
trainable_variables
regularization_losses
	keras_api
C__call__
*D&call_and_return_all_conditional_losses"м

_tf_keras_rnn_layerќ
{"class_name": "GRU", "name": "gru_1", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "stateful": false, "config": {"name": "gru_1", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.2, "recurrent_dropout": 0.4, "implementation": 1, "reset_after": false}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 1]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, null, 1]}}
ќ

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
E__call__
*F&call_and_return_all_conditional_losses"©
_tf_keras_layerП{"class_name": "Dense", "name": "dense_1", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "config": {"name": "dense_1", "trainable": true, "dtype": "float32", "units": 1, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 32}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 32]}}
Й
iter
	decay
learning_rate
momentum
rho	rms;	rms<	rms=	rms>	rms?"
	optimizer
C
0
1
2
3
4"
trackable_list_wrapper
C
0
1
2
3
4"
trackable_list_wrapper
 "
trackable_list_wrapper
 
metrics
non_trainable_variables

layers
 layer_regularization_losses
!layer_metrics
	variables
trainable_variables
regularization_losses
A__call__
@_default_save_signature
*B&call_and_return_all_conditional_losses
&B"call_and_return_conditional_losses"
_generic_user_object
,
Gserving_default"
signature_map
Г

kernel
recurrent_kernel
bias
"	variables
#trainable_variables
$regularization_losses
%	keras_api
H__call__
*I&call_and_return_all_conditional_losses"»
_tf_keras_layerЃ{"class_name": "GRUCell", "name": "gru_cell_1", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "config": {"name": "gru_cell_1", "trainable": true, "dtype": "float32", "units": 32, "activation": "tanh", "recurrent_activation": "hard_sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.2, "recurrent_dropout": 0.4, "implementation": 1, "reset_after": false}}
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
є
&metrics

'states
(non_trainable_variables

)layers
*layer_regularization_losses
+layer_metrics
	variables
trainable_variables
regularization_losses
C__call__
*D&call_and_return_all_conditional_losses
&D"call_and_return_conditional_losses"
_generic_user_object
 : 2dense_1/kernel
:2dense_1/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
≠
,metrics
-non_trainable_variables

.layers
/layer_regularization_losses
0layer_metrics
	variables
trainable_variables
regularization_losses
E__call__
*F&call_and_return_all_conditional_losses
&F"call_and_return_conditional_losses"
_generic_user_object
:	 (2RMSprop/iter
: (2RMSprop/decay
: (2RMSprop/learning_rate
: (2RMSprop/momentum
: (2RMSprop/rho
):'`2gru_1/gru_cell_1/kernel
3:1 `2!gru_1/gru_cell_1/recurrent_kernel
#:!`2gru_1/gru_cell_1/bias
'
10"
trackable_list_wrapper
 "
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
≠
2metrics
3non_trainable_variables

4layers
5layer_regularization_losses
6layer_metrics
"	variables
#trainable_variables
$regularization_losses
H__call__
*I&call_and_return_all_conditional_losses
&I"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
	0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
ї
	7total
	8count
9	variables
:	keras_api"Д
_tf_keras_metricj{"class_name": "Mean", "name": "loss", "dtype": "float32", "config": {"name": "loss", "dtype": "float32"}}
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
:  (2total
:  (2count
.
70
81"
trackable_list_wrapper
-
9	variables"
_generic_user_object
*:( 2RMSprop/dense_1/kernel/rms
$:"2RMSprop/dense_1/bias/rms
3:1`2#RMSprop/gru_1/gru_cell_1/kernel/rms
=:; `2-RMSprop/gru_1/gru_cell_1/recurrent_kernel/rms
-:+`2!RMSprop/gru_1/gru_cell_1/bias/rms
п2м
 __inference__wrapped_model_37012«
Л≤З
FullArgSpec
argsЪ 
varargsjargs
varkw
 
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *7Ґ4
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
ю2ы
,__inference_sequential_1_layer_call_fn_38510
,__inference_sequential_1_layer_call_fn_38541
,__inference_sequential_1_layer_call_fn_39266
,__inference_sequential_1_layer_call_fn_39251ј
Ј≤≥
FullArgSpec1
args)Ъ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsЪ
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
к2з
G__inference_sequential_1_layer_call_and_return_conditional_losses_38949
G__inference_sequential_1_layer_call_and_return_conditional_losses_39236
G__inference_sequential_1_layer_call_and_return_conditional_losses_38462
G__inference_sequential_1_layer_call_and_return_conditional_losses_38478ј
Ј≤≥
FullArgSpec1
args)Ъ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsЪ
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
ч2ф
%__inference_gru_1_layer_call_fn_40626
%__inference_gru_1_layer_call_fn_39946
%__inference_gru_1_layer_call_fn_40615
%__inference_gru_1_layer_call_fn_39935’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
г2а
@__inference_gru_1_layer_call_and_return_conditional_losses_40323
@__inference_gru_1_layer_call_and_return_conditional_losses_39643
@__inference_gru_1_layer_call_and_return_conditional_losses_39924
@__inference_gru_1_layer_call_and_return_conditional_losses_40604’
ћ≤»
FullArgSpecB
args:Ъ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsЪ

 
p 

 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
—2ќ
'__inference_dense_1_layer_call_fn_40645Ґ
Щ≤Х
FullArgSpec
argsЪ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
м2й
B__inference_dense_1_layer_call_and_return_conditional_losses_40636Ґ
Щ≤Х
FullArgSpec
argsЪ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsЪ 
kwonlydefaults
 
annotations™ *
 
6B4
#__inference_signature_wrapper_38566gru_1_input
Ь2Щ
*__inference_gru_cell_1_layer_call_fn_40909
*__inference_gru_cell_1_layer_call_fn_40923Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 
“2ѕ
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40895
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40794Њ
µ≤±
FullArgSpec3
args+Ъ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsЪ
p 

kwonlyargsЪ 
kwonlydefaults™ 
annotations™ *
 °
 __inference__wrapped_model_37012}AҐ>
7Ґ4
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
™ "1™.
,
dense_1!К
dense_1€€€€€€€€€Ґ
B__inference_dense_1_layer_call_and_return_conditional_losses_40636\/Ґ,
%Ґ"
 К
inputs€€€€€€€€€ 
™ "%Ґ"
К
0€€€€€€€€€
Ъ z
'__inference_dense_1_layer_call_fn_40645O/Ґ,
%Ґ"
 К
inputs€€€€€€€€€ 
™ "К€€€€€€€€€Ѕ
@__inference_gru_1_layer_call_and_return_conditional_losses_39643}OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ Ѕ
@__inference_gru_1_layer_call_and_return_conditional_losses_39924}OҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p 

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ Ї
@__inference_gru_1_layer_call_and_return_conditional_losses_40323vHҐE
>Ґ;
-К*
inputs€€€€€€€€€€€€€€€€€€

 
p

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ Ї
@__inference_gru_1_layer_call_and_return_conditional_losses_40604vHҐE
>Ґ;
-К*
inputs€€€€€€€€€€€€€€€€€€

 
p 

 
™ "%Ґ"
К
0€€€€€€€€€ 
Ъ Щ
%__inference_gru_1_layer_call_fn_39935pOҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p

 
™ "К€€€€€€€€€ Щ
%__inference_gru_1_layer_call_fn_39946pOҐL
EҐB
4Ъ1
/К,
inputs/0€€€€€€€€€€€€€€€€€€

 
p 

 
™ "К€€€€€€€€€ Т
%__inference_gru_1_layer_call_fn_40615iHҐE
>Ґ;
-К*
inputs€€€€€€€€€€€€€€€€€€

 
p

 
™ "К€€€€€€€€€ Т
%__inference_gru_1_layer_call_fn_40626iHҐE
>Ґ;
-К*
inputs€€€€€€€€€€€€€€€€€€

 
p 

 
™ "К€€€€€€€€€ Б
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40794Ј\ҐY
RҐO
 К
inputs€€€€€€€€€
'Ґ$
"К
states/0€€€€€€€€€ 
p
™ "RҐO
HҐE
К
0/0€€€€€€€€€ 
$Ъ!
К
0/1/0€€€€€€€€€ 
Ъ Б
E__inference_gru_cell_1_layer_call_and_return_conditional_losses_40895Ј\ҐY
RҐO
 К
inputs€€€€€€€€€
'Ґ$
"К
states/0€€€€€€€€€ 
p 
™ "RҐO
HҐE
К
0/0€€€€€€€€€ 
$Ъ!
К
0/1/0€€€€€€€€€ 
Ъ Ў
*__inference_gru_cell_1_layer_call_fn_40909©\ҐY
RҐO
 К
inputs€€€€€€€€€
'Ґ$
"К
states/0€€€€€€€€€ 
p
™ "DҐA
К
0€€€€€€€€€ 
"Ъ
К
1/0€€€€€€€€€ Ў
*__inference_gru_cell_1_layer_call_fn_40923©\ҐY
RҐO
 К
inputs€€€€€€€€€
'Ґ$
"К
states/0€€€€€€€€€ 
p 
™ "DҐA
К
0€€€€€€€€€ 
"Ъ
К
1/0€€€€€€€€€ ƒ
G__inference_sequential_1_layer_call_and_return_conditional_losses_38462yIҐF
?Ґ<
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
p

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ ƒ
G__inference_sequential_1_layer_call_and_return_conditional_losses_38478yIҐF
?Ґ<
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
p 

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ њ
G__inference_sequential_1_layer_call_and_return_conditional_losses_38949tDҐA
:Ґ7
-К*
inputs€€€€€€€€€€€€€€€€€€
p

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ њ
G__inference_sequential_1_layer_call_and_return_conditional_losses_39236tDҐA
:Ґ7
-К*
inputs€€€€€€€€€€€€€€€€€€
p 

 
™ "%Ґ"
К
0€€€€€€€€€
Ъ Ь
,__inference_sequential_1_layer_call_fn_38510lIҐF
?Ґ<
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
p

 
™ "К€€€€€€€€€Ь
,__inference_sequential_1_layer_call_fn_38541lIҐF
?Ґ<
2К/
gru_1_input€€€€€€€€€€€€€€€€€€
p 

 
™ "К€€€€€€€€€Ч
,__inference_sequential_1_layer_call_fn_39251gDҐA
:Ґ7
-К*
inputs€€€€€€€€€€€€€€€€€€
p

 
™ "К€€€€€€€€€Ч
,__inference_sequential_1_layer_call_fn_39266gDҐA
:Ґ7
-К*
inputs€€€€€€€€€€€€€€€€€€
p 

 
™ "К€€€€€€€€€і
#__inference_signature_wrapper_38566МPҐM
Ґ 
F™C
A
gru_1_input2К/
gru_1_input€€€€€€€€€€€€€€€€€€"1™.
,
dense_1!К
dense_1€€€€€€€€€