"""Add annotated_class column

Revision ID: 48db545014b9
Revises: d3c66414bf63
Create Date: 2020-03-23 16:39:13.558787

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '48db545014b9'
down_revision = 'd3c66414bf63'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('object_contexts', Column('annotated_cls', String(200)))
    pass


def downgrade():
    pass
