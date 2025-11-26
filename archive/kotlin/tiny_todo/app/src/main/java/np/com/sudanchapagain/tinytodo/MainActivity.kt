package np.com.sudanchapagain.tinytodo

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.viewModels
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.Spring
import androidx.compose.animation.core.spring
import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.launch
import np.com.sudanchapagain.tinytodo.ui.theme.TinyTodoTheme
import np.com.sudanchapagain.tinytodo.view.TaskViewModel

data class Task(val title: String, val isCompleted: Boolean)

enum class Filter {
    All, Active
}

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()

        val viewModel: TaskViewModel by viewModels()
        setContent {
            TinyTodoTheme {
                Scaffold(modifier = Modifier.fillMaxSize()) { paddingValues ->
                    TodoApp(
                        modifier = Modifier.padding(paddingValues), viewModel = viewModel
                    )
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TodoApp(modifier: Modifier = Modifier, viewModel: TaskViewModel) {
    val tasks by viewModel.tasks.collectAsState()
    var filter by remember { mutableStateOf(Filter.All) }
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)
    var showBottomSheet by remember { mutableStateOf(false) }

    if (showBottomSheet) {
        ModalBottomSheet(
            onDismissRequest = { showBottomSheet = false }, sheetState = sheetState
        ) {
            AddTaskBottomSheet(
                onAdd = { title ->
                    viewModel.addTask(title)
                    showBottomSheet = false
                })
        }
    }

    Column(modifier = modifier.padding(16.dp)) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(bottom = 8.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.SpaceBetween
        ) {
            Text("tiny todo", fontWeight = FontWeight.Black, style = MaterialTheme.typography.titleLarge)
            Row {
                FilterOption("All", Filter.All, filter) { filter = it }
                FilterOption("Active", Filter.Active, filter) { filter = it }
            }
        }

        LazyColumn(modifier = Modifier.weight(1f)) {
            items(items = tasks.filter { filter == Filter.All || !it.isCompleted }, key = { it.id }) { taskEntity ->
                val task = Task(title = taskEntity.title, isCompleted = taskEntity.isCompleted)
                SwipeToDeleteTaskItem(task = task, onCheckedChange = { isChecked ->
                    viewModel.updateTask(taskEntity.copy(isCompleted = isChecked))
                }, onDelete = { viewModel.deleteTask(taskEntity) })
            }
        }

        Button(
            onClick = { showBottomSheet = true }, modifier = Modifier.align(Alignment.CenterHorizontally)
        ) {
            Text(
                modifier = Modifier.padding(
                    start = 16.dp, end = 16.dp
                ),
                text = "Add task",
            )
        }
    }
}

@Composable
fun AddTaskBottomSheet(onAdd: (String) -> Unit) {
    var title by remember { mutableStateOf("") }

    Column(modifier = Modifier.padding(16.dp)) {
        Text(
            "Add task",
            style = MaterialTheme.typography.titleLarge,
            fontWeight = FontWeight.SemiBold,
        )
        Spacer(modifier = Modifier.height(8.dp))
        OutlinedTextField(
            value = title,
            onValueChange = { title = it },
            singleLine = true,
            placeholder = { Text("enter title") },
            modifier = Modifier.fillMaxWidth()
        )
        Spacer(modifier = Modifier.height(16.dp))
        Button(
            onClick = { if (title.isNotEmpty()) onAdd(title) }, modifier = Modifier.align(Alignment.End)
        ) {
            Text(
                modifier = Modifier.padding(
                    start = 16.dp, end = 16.dp
                ), text = "Add"
            )
        }
    }
}

@Composable
fun SwipeToDeleteTaskItem(
    task: Task, onCheckedChange: (Boolean) -> Unit, onDelete: () -> Unit
) {
    val offsetX = remember { Animatable(0f) }
    val deleteThreshold = -420f

    val coroutineScope = rememberCoroutineScope()

    Box(
        modifier = Modifier
            .fillMaxWidth()
            .background(Color.Red.copy(alpha = 0.2f))
            .pointerInput(Unit) {
                detectHorizontalDragGestures(onDragEnd = {
                    coroutineScope.launch {
                        if (offsetX.value < deleteThreshold) {
                            offsetX.animateTo(
                                targetValue = -1000f, animationSpec = spring(stiffness = Spring.StiffnessLow)
                            )
                            onDelete()
                        } else {
                            offsetX.animateTo(
                                targetValue = 0f, animationSpec = spring(stiffness = Spring.StiffnessMedium)
                            )
                        }
                    }
                }, onHorizontalDrag = { change, dragAmount ->
                    change.consume()
                    coroutineScope.launch {
                        offsetX.snapTo(offsetX.value + dragAmount)
                    }
                })
            }) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
                .offset { IntOffset(offsetX.value.toInt(), 0) }
                .fillMaxWidth()
                .background(MaterialTheme.colorScheme.background)
                .padding(12.dp)) {
            Checkbox(
                checked = task.isCompleted, onCheckedChange = onCheckedChange, colors = CheckboxDefaults.colors(
                    checkedColor = Color.Gray, uncheckedColor = MaterialTheme.colorScheme.onSurface
                ), modifier = Modifier.size(24.dp)
            )
            Spacer(modifier = Modifier.width(8.dp))
            Text(
                task.title, style = MaterialTheme.typography.bodyLarge, modifier = Modifier.weight(1f)
            )
        }
    }
}

@Composable
fun FilterOption(label: String, option: Filter, selectedFilter: Filter, onFilterSelected: (Filter) -> Unit) {
    TextButton(onClick = { onFilterSelected(option) }) {
        Text(
            text = label, fontWeight = if (option == selectedFilter) FontWeight.Bold else FontWeight.Normal
        )
    }
}
